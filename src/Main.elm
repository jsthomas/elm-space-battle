module Main exposing (main)

import Browser
import Browser.Events

import Canvas exposing (rect, shapes, path, lineTo, circle, text, Renderable)
import Canvas.Settings exposing (fill, Setting)
import Canvas.Settings.Text exposing (font, align)
import Canvas.Settings.Advanced exposing (rotate, transform, translate)
import Color
import Html exposing (Html, div)
import Html.Attributes exposing (style)

import Json.Decode
import Canvas.Settings.Text exposing (TextAlign(..))
import String exposing (toInt)

width : Int
width = 800

height : Int
height = 800

centerX : Float
centerX =
    toFloat width / 2

centerY : Float
centerY =
    toFloat height / 2

-- Position is given in coordinates relative to the Sun.
type alias Position = (Float, Float)
type alias Velocity = (Float, Float)
type alias Orientation = Float

type GameState = Start | Playing | Win | Lose String

type alias Player =
    { position: Position
    , theta: Orientation
    , velocity: Velocity
    }

type alias Enemy =
    { position: Position
    , theta: Orientation
    , velocity: Velocity
    }

type alias Torpedo =
    { position : Position
    , velocity : Velocity
    , created : Float
    , fromPlayer : Bool 
    }

type alias Model =
    {
      state : GameState
    , time : Float
    , torpedos : List Torpedo
    , player : Player
    , enemies : List Enemy }

type Direction = Left | Right

type Msg = Frame Float
    | Accelerate
    | Rotate Direction
    | Shoot
    | Restart
    | NoOp


updatePosition : Position -> Velocity -> Position
updatePosition (px, py) (dx, dy) =
    let wrap x m = if x < -m then wrap (x + 2 * m) m
            else if m < x then wrap (x - 2 * m) m
            else x in
    let wrapCoords (x, y) = (wrap x (toFloat width / 2), wrap y (toFloat height / 2)) in
    wrapCoords (px + dx, py + dy)


clampV : Velocity -> Velocity
clampV (dx, dy) =
    let maxVelocity = 5 in
    let toPolar (x, y) = (sqrt (x^2 + y^2), atan2 y x) in
    let (r, theta) = toPolar (dx, dy) in
    if r < maxVelocity then (dx, dy) else (maxVelocity * cos theta, maxVelocity * sin theta)
 

updateVelocity : Position -> Velocity -> Velocity
updateVelocity (x, y) (dx, dy) =
    let gravity = 350 in
    let factor = gravity / (x^2 + y^2) ^ 1.5 in
    clampV (dx - factor * x, dy - factor * y )


accelerate : Player -> Player
accelerate p =
    let accelFactor = 1 in
    let (dx, dy) = p.velocity in
    let vNext = clampV (dx + accelFactor * cos p.theta, dy + accelFactor * sin p.theta) in
    {p | velocity = vNext}


updatePlayer : Player -> Player
updatePlayer p =
    let pNext = updatePosition p.position p.velocity in
    let vNext = updateVelocity p.position p.velocity in
    { p | position = pNext, velocity = vNext}

updateTorpedo : Torpedo -> Torpedo
updateTorpedo b =
    let pNext = updatePosition b.position b.velocity in
    let vNext = updateVelocity b.position b.velocity in
    {b | position = pNext, velocity = vNext}


rotatePlayer : Direction -> Player -> Player
rotatePlayer dir p =
    let rotationRate = 10 in
    let amount = if dir == Left then degrees -rotationRate else degrees rotationRate in
    { p | theta = p.theta + amount }


distance : Position -> Position -> Float
distance (x1, y1) (x2, y2) = sqrt((x1 - x2)^2 + (y1 - y2)^2)


facePoint : Position -> Enemy -> Enemy
facePoint p e =
    -- Make the enemy orient itself toward the player.
    let (px, py) = p in
    let (ex, ey) = e.position in
    let theta = atan2 (py - ey) (px - ex) in
    {e | theta = theta}

moveEnemy : Enemy -> Enemy
moveEnemy e = 
    -- Enemies travel along simple orbits, tracking the player's position.
    -- Enemies further from the sun travel on slower orbits.
    let deltaOmega = degrees 1 in
    let toPolar (x, y) = (sqrt (x^2 + y^2), atan2 y x) in
    let (r, omega) = toPolar e.position in
    let omegaNext = omega + deltaOmega * (toFloat width) / (4 * r) in
    { e | position = (r * cos omegaNext, r * sin omegaNext)} 


makeEnemyTorpedos : Float -> List Enemy -> List Torpedo
makeEnemyTorpedos time enemies =
    let shoot i e = if i * 300 == modBy 900 (truncate time)
                    then [makeTorpedo False time e.position e.theta]
                    else [] in
    enemies |> List.indexedMap shoot |> List.concat 


updateEnemies : Model -> Model
updateEnemies model =
    let d b e = distance b.position e.position in
    let dSun b = distance b.position (0, 0) in 
    let closest x others = List.map (\ e -> d e x) others |> List.minimum |> (Maybe.withDefault 100.0)
    in
    let enemiesNext = List.filter (\ e -> (closest e model.torpedos) > 10) model.enemies in
    let torpedosNext = List.filter (\ b -> (closest b model.enemies) > 10 && dSun b > 10) model.torpedos in
    let enemiesMotion = List.map ((facePoint model.player.position) >> moveEnemy) enemiesNext in
    let newTorpedos = makeEnemyTorpedos model.time enemiesMotion in
    { model | torpedos = (torpedosNext ++ newTorpedos), enemies = enemiesMotion }


updateGameState : Model -> GameState
updateGameState {player, torpedos, enemies} =
    let torpedo_hits = List.any (\ b -> distance player.position b.position < 10) torpedos in
    let enemy_hits = List.any (\ e -> distance player.position e.position < 10) enemies in
    let sun_hit = distance player.position (0, 0) < 10 in
    if torpedo_hits then Lose "You were destroyed by a torpedo!" else
    if enemy_hits then Lose "You were destroyed in a collision!" else
    if sun_hit then Lose "Your ship was consumed by a star!" else
    if List.length enemies == 0 then Win else
    Playing


makeTorpedo : Bool -> Float -> Position -> Orientation -> Torpedo
makeTorpedo fromPlayer time (x, y) theta =
    let torpedoSpeed = 3.0 in
    let psn = (x + 20 * cos theta, y + 20 * sin theta) in
    Torpedo psn (torpedoSpeed * cos theta, torpedoSpeed * sin theta) time fromPlayer


maxPlayerTorpedos : Int
maxPlayerTorpedos = 5

addPlayerTorpedo : Model -> Model
addPlayerTorpedo model =
    let count p l = (List.filter p l) |> List.length in
    let currentPlayerTorpedos = count .fromPlayer model.torpedos in
    if currentPlayerTorpedos >= maxPlayerTorpedos then
        model
    else
        let b = makeTorpedo True model.time model.player.position model.player.theta in 
        { model | torpedos=b :: model.torpedos }


removeOldTorpedos : Model -> Model
removeOldTorpedos model =
    let torpedoLifetime = 60 * 2 in  -- ~ 2 seconds
    let isLive b = model.time - b.created < torpedoLifetime in
    {model | torpedos = List.filter isLive model.torpedos}

update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    if model.state == Start then
        case msg of 
            Restart -> (init Playing, Cmd.none)
            _ -> (model, Cmd.none)
    else
    if model.state /= Playing && msg /= Restart then (model, Cmd.none) else
    case msg of
        Accelerate ->
            ({ model | player = accelerate model.player }, Cmd.none)
        Rotate dir ->
            ({ model | player = rotatePlayer dir model.player }, Cmd.none)
        Shoot ->
            (addPlayerTorpedo model, Cmd.none)
        Frame _ ->
            let m = model 
                    |> removeOldTorpedos
                    |> updateEnemies in
            (
                { state = updateGameState m
                , time = m.time + 1
                , player = updatePlayer m.player
                , enemies = m.enemies
                , torpedos = List.map updateTorpedo m.torpedos }
                , Cmd.none
            )
        Restart -> (init Playing, Cmd.none)
        NoOp -> (model, Cmd.none)


keyDecoder : Json.Decode.Decoder Msg
keyDecoder =
  Json.Decode.map toDirection (Json.Decode.field "key" Json.Decode.string)


toDirection : String -> Msg
toDirection string = let _ = Debug.log "Pressed:" string in
  case string of
    "w" -> Accelerate
    "a" -> Rotate Left
    "d" -> Rotate Right
    "s" -> Shoot
    " " -> Shoot
    "Enter" -> Restart
    _ -> NoOp


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch <|
        [
            Browser.Events.onKeyPress keyDecoder
            , Browser.Events.onAnimationFrameDelta Frame
        ]


init : GameState -> Model
init s =
    let polar r t = (r * cos (degrees t), r * sin (degrees t)) in
    let p = { position= polar 400 135, theta=(degrees 45), velocity=(0, 0) } in
    let e1 = { position= polar 300 315, theta=(degrees 0), velocity=(0, 0) } in
    let e2 = { position=polar 100 0, theta=(degrees 0), velocity=(0, 0) } in
    let e3 = { position=polar 200 0, theta=(degrees 0), velocity=(0, 0) } in
    {
        state = s
        , player = p
        , enemies = List.map (facePoint p.position) [e1, e2, e3]
        , torpedos = []
        , time = 0
    }

main : Program () Model Msg
main =
    Browser.element
        { init = \ _ -> (init Start, Cmd.none)
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


view : Model -> Html Msg
view model =
    let torpedos = List.map (renderTorpedo model.time) model.torpedos in
    let player = renderPlayer model.player in
    let enemies = List.map renderEnemy model.enemies in
    let text = textBox model.state in
    let score = header model in

    let title = case model.state of
                Start -> titleCard
                _ -> []
    in
    -- Later renderings cover up earlier ones in this list.
    let renders = [ clearScreen, background, player] 
                    ++ enemies ++ torpedos ++ [sun] ++ score ++ text ++ title in
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtml
            ( width, height )
            [ style "border" "10px solid rgba(0,0,0,0.1)" ]
            renders
        ]


clearScreen : Renderable
clearScreen =
    shapes [ fill Color.black ] [ rect ( 0, 0 ) (toFloat width) (toFloat height) ]


starPositions : List (number, number)
starPositions = [
    (115, 155), (349, 433), (673, 244), (197, 705), (285, 419), (620, 371),
    (197, 702), (189, 776), (91, 320), (195, 223), (342, 778), (113, 726),
    (573, 291), (48, 435), (120, 721), (788, 591), (531, 21), (13, 20),
    (351, 5), (130, 385), (66, 424), (322, 752), (383, 330), (85, 364),
    (677, 602), (792, 650), (732, 496), (730, 380), (421, 115), (611, 70),
    (735, 157), (447, 356), (112, 6), (626, 74), (486, 666), (416, 793),
    (141, 302), (407, 45), (779, 750), (754, 265), (706, 592), (258, 71),
    (448, 258), (458, 782), (771, 767), (751, 705), (239, 186), (542, 381),
    (696, 305), (148, 402)]


background : Renderable
background =
    let starSize = 2 in
    shapes [ fill Color.white ]
        (List.map (\ p -> rect p starSize starSize) starPositions)


gameFont : Int -> List Setting
gameFont size = 
    [ fill Color.white
    , font { size = size, family = "sans-serif" }
    , align Center ]


textBox : GameState -> List Renderable
textBox g =
    let positions = [(centerX, centerY - 200), (centerX, centerY - 100)] in
    let lines = case g of
                Start -> []
                Playing -> []
                Win -> ["You Won!", "Press Enter to play again."]
                Lose reason -> [reason, "Press Enter to play again."] in
    List.map2 (text (gameFont 24)) positions lines

header : Model -> List Renderable
header model = 
    let playerTorpedos = (List.filter .fromPlayer model.torpedos) 
                        |> List.length 
                        |> (\x -> maxPlayerTorpedos - x)
                        |> String.fromInt in
    let enemies = model.enemies |> List.length |> String.fromInt in
    let positions = [(centerX - 300, 50), (centerY + 300, 50)] in
    let lines = 
            [ "Enemies: " ++ enemies
            , "Torpedoes: " ++ playerTorpedos ] in
    List.map2 (text (gameFont 18)) positions lines


titleCard : List Renderable
titleCard = 
    let offset u = (centerX, centerY - u) in
    let title = text (gameFont 24) (offset 250) "Space Battle!" in
    let positions = List.map offset [200, 175, 150, 125] in
    let lines = [ "A / D: Rotate the ship left/right."
                , "W: Fire thrusters."
                , "S / Space: Launch a torpedo."
                , "Enter: Play / Restart" ] in
    let instructions = List.map2 (text (gameFont 18)) positions lines in
    title :: instructions


sun : Renderable
sun =
    let sunSize = 15 in
    shapes [ fill Color.yellow ] [ circle (centerX, centerY) sunSize ]


renderTorpedo : Float -> Torpedo -> Renderable
renderTorpedo time {position} =
    let torpedoSize = 3 in
    let transl (x, y) = translate (x + centerX) (y + centerY) in
    let hue =
            (time |> floor |> modBy 25 |> toFloat) / 100 in
    shapes [
        fill (Color.hsl 0 0.7 (0.5 + hue))
        , transform [ transl position ]
    ] [ circle (0, 0) torpedoSize ]


renderEnemy : Enemy -> Renderable
renderEnemy e =
    let transl (x, y) = translate (x + centerX) (y + centerY) in
    shapes [ transform [ transl e.position, rotate e.theta],
             fill Color.blue ]
             [ path (20, 0) [lineTo (-20, 5), lineTo (-20, -5) ],
             path (0, 0) [lineTo (-20, 10), lineTo (-20, -10) ] ]


renderPlayer : Player -> Renderable
renderPlayer p =
    let transl (x, y) = translate (x + centerX) (y + centerY) in
    shapes [ transform [ transl p.position, rotate p.theta ],
                fill Color.purple ]
                [ path (20, 0) [lineTo (-20, 10), lineTo (-20, -10) ] ]
