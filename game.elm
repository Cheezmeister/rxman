import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
-- import Markdown
import Time exposing (..)
import Window
import String
import List exposing (map)
import Random exposing (Seed, generate, initialSeed)
import Maybe exposing (withDefault)


-- MODEL

areaW = 800
areaH = 600

bottom = -areaH/2 + 28

type alias Debuggable = Float

type alias Input = (Time, { x:Int, y:Int }, Bool)

type ThingKind = Grow | Shrink | Shroom
type alias Thing = 
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , kind : ThingKind 
  }

type alias Model =
  { player : Player
  , stuff : List Thing
  , ticks : Float
  }

type alias Player =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , size : Float
  }


player : Player
player =
  Player 0 0 0 0 1.0

model = Model player [] 0

sign : Float -> Float
sign n =
  if n < 0 then
    -1
  else if n > 0 then
    1
  else
    0

-- UPDATE

update : Input -> Model -> Model
update (timeDelta, direction, isRunning) model =
  let 
      newplayer = updatePlayer (timeDelta, direction, isRunning) model.player
      newstuff = model.stuff
        |> spawnStuff (model.ticks, timeDelta)
        |> moveStuff (timeDelta)
        |> List.filter (isOnScreen newplayer)
  in
    Model newplayer newstuff (model.ticks + Time.inMilliseconds timeDelta)
        |> collide
  -- TODO why this no work
  -- { model |
  --     player = updatePlayer (timeDelta, direction, isRunning) player
  -- }


spawnStuff : (Float, Time) -> List Thing -> List Thing
spawnStuff (ticks, timeDelta) stuff =
  let
    (roll, seed) = generate (Random.float 0 100) (initialSeed (round ticks))
  in
    if roll < timeDelta then
      spawnThing seed :: stuff
    else
      stuff


spawnThing : Seed -> Thing 
spawnThing seed =
  let
      (x, seed2) = generate (Random.float (-areaW/2) (areaW/2)) seed
      (vx, seed3) = generate (Random.float -5 5) seed2
      (vy, seed4) = generate (Random.float 2 8) seed3
      (b, seed5) = generate (Random.int 0 2) seed4
      kind = case b of
        0 -> Grow
        1 -> Shrink
        _ -> Shroom
  in
    Thing x (areaH*1.5) vx vy kind


moveStuff : (Time) -> List Thing -> List Thing
moveStuff (timeDelta) stuff =
  stuff |> map (\thing -> 
    { thing |
      y = thing.y - thing.vy * timeDelta
    }
  )


updatePlayer : Input -> Player -> Player
updatePlayer (timeDelta, direction, isRunning) player =
  player
    |> newVelocity isRunning direction
    |> updatePosition timeDelta


newVelocity : Bool -> { x:Int, y:Int } -> Player -> Player
newVelocity isRunning {x,y} model =
  let
    scale =
      -- if isRunning then 2 else 1
      10

    drag = 0.8
    accel = 0.2

    newVel n =
      if n == 0 then
        model.vx - drag * sign model.vx
      else
        model.vx + accel * scale * toFloat n
          |> clamp -scale scale
  in
      { model |
          vx = newVel x
         ,vy = 1
      }


updatePosition : Time -> Player -> Player
updatePosition dt ({x,y,vx,vy} as model) =
  { model |
       x = (x + dt * vx) |> clamp (-areaW/2) (areaW/2)
     , y = (y + dt * vy)
  }


isOnScreen player entity =
  entity.y > -areaW/2


collide : Model -> Model
collide model =
  let
    overlap player thing =
      (abs (thing.x - player.x) < player.size*11) && (abs (thing.y - bottom) < player.size*14)
    colliders player stuff =
      List.filter (overlap player) stuff
    noncolliders player stuff =
      List.filter (not << overlap player) stuff
    growOrShrink multiplier player =
      { player | size = player.size * multiplier }
    handleCollision player stuff =
      case (colliders player stuff |> List.head) of
        Nothing -> growOrShrink 1
        Just thing -> case thing.kind of
          Grow -> growOrShrink 1.5
          Shrink -> growOrShrink 0.5
          Shroom -> growOrShrink 1

    player = model.player
    stuff = model.stuff

    affect = handleCollision player stuff
  in
    { model |
         player = affect player
       , stuff = noncolliders player stuff
    }


-- VIEW

debugexpr : (String, Debuggable) -> String
debugexpr (name, expr) =
  name ++ "=" ++ toString (expr)

debug : List (String, Debuggable) -> Form
debug vars =
  List.map debugexpr vars
    |> String.join ", "
    |> show
    |> toForm
    |> move (0, areaH/2 - 16)

view : (Int,Int) -> Model -> Element
view (w,h) model =
  let
    offset = toFloat -(round y % areaH)
    {x,y,vx,vy,size} = model.player
  in
    container w h middle <|
    collage areaW areaH <|
      -- Background
      [ toForm (tiledImage areaW areaH "imgs/desert.png")
         |> move (0, offset)
      , toForm (tiledImage areaW areaH "imgs/desert.png")
         |> move (0, areaH + offset)

      -- Player
      , (toForm (image 22 28 "imgs/player.png"))
          |> scale size
          |> rotate (degrees (vx * -5))
          |> move (x, bottom)

      -- , debug [("x", x), ("y", y), ("ticks", model.ticks)]

      ] ++ (drawStuff model.ticks model.stuff)

drawStuff : Float -> List Thing -> List Form
drawStuff ticks stuff =
  stuff |> List.map (drawThing ticks)

drawThing : Float -> Thing -> Form
drawThing ticks {x, y, kind} =
  let
    img = case kind of
        Grow -> "plus"
        Shrink -> "minus"
        Shroom -> "shroom"
  in
    toForm (image 32 32 ("imgs/" ++ img ++ ".png"))
      |> rotate (degrees ticks)
      |> move (x, y)


-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update model input)


input : Signal Input
input =
  Signal.sampleOn delta (Signal.map3 (,,) delta Keyboard.arrows Keyboard.shift)


delta : Signal Time
delta =
  Signal.map (\t -> t / 20) (fps 25)

