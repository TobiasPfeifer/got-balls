module Main exposing (main)

import Basics as Math
import Browser
import Browser.Events exposing (onAnimationFrameDelta, onMouseMove)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseEnter)
import Html.Events.Extra.Mouse as Mouse
import Point2d exposing (unitless)
import Quantity exposing (Quantity, Unitless)
import Svg exposing (Svg, circle, rect, svg)


import Svg.Attributes exposing (color, fill, height, viewBox, width, x, y)
import Time
import Random
import Tuple exposing (first, second)


main = Browser.element { init = init,  view = view, update = update, subscriptions = subscriptions }

type alias Ball = { x: Int, y: Float, size: Int, speed: Float, color: String, clicked: Bool, dead: Bool, id: Int }
type State = INITIAL | RUNNING | OVER
type alias Model = { state: State, balls: List Ball, score: Int, lifes: Int, lastTick: Time.Posix, lastAddBall: Int, spawnTiming: Float, mousePos: (Float, Float)}

init : () -> (Model, Cmd Msg)
init _ = ({state = INITIAL, balls = [], score = 0, lifes = 10, lastTick = (Time.millisToPosix 0), lastAddBall = 0, spawnTiming = 1000, mousePos = (0,0)}, Cmd.none)

type alias XPos = Int
type alias YPos = Float
type alias Size = Int
type alias Speed = Float

type alias BallProps = {pos: (XPos, YPos), size: Size, speed: Speed, id: Int}

type Msg = MoveMsg (Float, Float) | Log | Start | Quit | Frame Float | AddBall BallProps  | Spawn Time.Posix | Kill Int | Increment | Decrement

subscriptions : Model -> Sub Msg
subscriptions model = case model.state of
    RUNNING -> Sub.batch [onAnimationFrameDelta Frame, Time.every model.spawnTiming Spawn]
    INITIAL -> Sub.none
    OVER -> Sub.none


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Increment ->
      (model, Cmd.none)

    Decrement ->
      (model, Cmd.none)

    Log ->
      (model, Cmd.none)

    Start ->
        ({model | state = RUNNING}, Cmd.none)

    Quit ->
        ({model | state = OVER}, Cmd.none)

    Frame delta ->
        let movedModel = {model | balls = updateBalls delta model.balls} in
        let updatedKillsModel = updateKills movedModel in
        let updatedModel = markLostBallsModel updatedKillsModel in
        (updatedModel, Cmd.none)

    Spawn posix ->
        (model , Random.generate AddBall (genBallProps model))

    AddBall {pos, size, speed, id} ->
        let (posX, posY) = pos in
        ({model | balls = addBall model.balls id posX posY size speed, spawnTiming = (Math.max 50 model.spawnTiming - 3)}, Cmd.none)

    Kill id ->
        let log = Debug.log "killed" id in
        ({model | balls = kill id model.balls}, Cmd.none)

    MoveMsg mousePos ->
        ({model | mousePos = mousePos}, Cmd.none)


updateKills: Model -> Model
updateKills model = {model | balls = List.map (\ball -> {ball | clicked = ball.clicked || (inside ball model.mousePos)}) model.balls}

inside: Ball -> (Float, Float) -> Bool
inside ball (x,y) = toFloat ball.x <= x && toFloat (ball.x + ball.size) >= x && ball.y <= y && (ball.y + toFloat ball.size) >= y

markLostBallsModel: Model -> Model
markLostBallsModel model = {model | balls = List.map (\ball -> {ball | dead = ball.dead || ball.y > 600}) model.balls}

kill: Int -> List Ball -> List Ball
kill id balls = List.map (\ball -> {ball | clicked = (ball.clicked || (not ball.dead && ball.id == id))}) balls

genBallProps: Model -> Random.Generator BallProps
genBallProps model =
    let minSize = Math.max 32 (100 - List.length model.balls) in
    let minSpeed = Math.min 50 (5 * List.length model.balls) in
    let maxSpeed = minSpeed + 10 in
    Random.map4 (\posX posY size speed -> {pos = (posX, posY), size = size, speed = speed, id = List.length model.balls})
        (Random.int 0 (800- minSize))
        (Random.float 0 20)
        (Random.int minSize (minSize + 15))
        (Random.float (Math.toFloat minSpeed) (Math.toFloat maxSpeed))

updateBalls: Float -> List Ball -> List Ball
updateBalls delta balls = List.map (updateBall delta) balls

updateBall: Float -> Ball -> Ball
updateBall delta ball = {ball | y = if (isAlive ball) then ball.y + (ball.speed * 1 / 5.0) else ball.y}

addBall: List Ball -> Int -> XPos -> YPos -> Size -> Speed -> List Ball
addBall balls id x y size speed = { x = x, y = y, size = size, speed = speed, color = "blue", clicked = False, dead = False, id = id} :: balls

random: Random.Generator Int
random = Random.int 2 800

view: Model -> Html Msg
view = viewWith renderDiv
--view = viewWith renderSvg

viewWith: (List Ball -> List (Html Msg)) -> Model -> Html Msg
viewWith func model =
      div []
        [ button [ onClick Log ] [ text "LogMe" ]
        , button [ onClick Start ] [ text "Start New Game" ]
        , button [ onClick Quit ] [ text "Quit Game" ]
        , div [
                Mouse.onMove (.offsetPos >> MoveMsg),
                --Mouse.onLeave (.clientPos >> MoveMsg),
                style "position" "absolute",
                style "cursor" "crosshair",
                style "overflow" "hidden",
                style "top" "50%",
                style "left" "50%",
                style "transform" "translate(-50%, -50%)",
                style "border" "3px solid black",
                style "max-height" "600px",
                style "max-width" "800px",
                style "min-height" "600px",
                style "min-width" "800px"
                ] ((func model.balls)
                 --:: [pointerDiv model]
                )
        ]

pointerDiv: Model -> Html Msg
pointerDiv model = (div [ style "position"            "absolute",
                                      style "pointer-events" "none",
                                                                              style "height"              ("30px"),
                                                                              style "width"               ("30px"),
                                                                              style "background-color"    ("red"),
                                                                              style "left"                (Debug.log "x " (String.fromInt (Math.floor (first model.mousePos)) ++ "px")),
                                                                              style "top"                 (Debug.log "y " (String.fromInt (Math.floor (second model.mousePos)) ++ "px"))
                                                                              --onClick (Kill ball.id),
                                                                              --onMouseEnter (Kill ball.id)
                                                                              ] [])

renderSvg: List Ball -> List (Html Msg)
renderSvg balls = [svg
                        [ width "800", height "600", viewBox "0 0 800 600" ]
                        (List.map renderBallSVG (List.filter isAlive balls))
                        --[ rect [ x "10", y "10", width "100", height "100", rx "15", ry "15" ] [] ]
                        ]

renderBallSVG: Ball -> Svg Msg
renderBallSVG ball = rect [
                        --onMouseEnter (Kill ball.id),
                        x (String.fromInt ball.x),
                        y (String.fromFloat ball.y),
                        width (String.fromInt ball.size),
                        height (String.fromInt ball.size),
                        fill ball.color] []

renderDiv: List (Ball) -> List (Html Msg)
renderDiv balls = List.map renderBallDiv (List.filter isAlive balls)

isAlive: Ball -> Bool
isAlive {clicked, dead} = not clicked && not dead

renderBallDiv: Ball -> Html Msg
renderBallDiv ball = div [ style "position"            "absolute",
                        style "pointer-events" "none",
                        style "height"              (String.fromInt ball.size ++ "px"),
                        style "width"               (String.fromInt ball.size ++ "px"),
                        style "background-color"    (ball.color),
                        style "left"                (String.fromInt ball.x ++ "px"),
                        style "top"                 (String.fromInt (Math.floor ball.y) ++ "px")
                        --onClick (Kill ball.id),
                        --onMouseEnter (Kill ball.id)
                        ] []
