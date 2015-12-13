import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
-- import Markdown
import Time exposing (..)
import Window


-- MODEL

areaW = 400
areaH = 300

type alias Input = (Time, { x:Int, y:Int }, Bool)

type alias Model =
  { hero : Player
  -- , stuff : [Thing]
  }

type alias Player =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , dir : String
  }


hero : Player
hero =
  Player 0 0 0 0 "south"

model = Model hero

sign : Float -> Float
sign n =
  if n < 0 then
    -1
  else if n > 0 then
    1
  else
    0

-- UPDATE

update : (Time, { x:Int, y:Int }, Bool) -> Model -> Model
update (timeDelta, direction, isRunning) model =
  { model |
      hero = updatePlayer (timeDelta, direction, isRunning) hero
  }


updatePlayer : (Time, { x:Int, y:Int }, Bool) -> Player -> Player
updatePlayer (timeDelta, direction, isRunning) hero =
  hero
    |> newVelocity isRunning direction
    |> setDirection direction
    |> updatePosition timeDelta


newVelocity : Bool -> { x:Int, y:Int } -> Player -> Player
newVelocity isRunning {x,y} model =
  let
    scale =
      -- if isRunning then 2 else 1
      5

    drag = 0.4
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


setDirection : { x:Int, y:Int } -> Player -> Player
setDirection {x,y} model =
  { model |
      dir = "west"
  }


updatePosition : Time -> Player -> Player
updatePosition dt ({x,y,vx,vy} as model) =
  { model |
      x = (x + dt * vx) |> clamp (-areaW/2) (areaW/2),
      y = (y + dt * vy)
  }


-- VIEW

view : (Int,Int) -> Model -> Element
view (w,h) model =
  let
    verb = if vx == 0 && vy == 0 then "stand" else "walk"
    src = "/imgs/UFO.gif"
    offset = toFloat -(round y % areaH)
    {x,y,vx,vy,dir} = model.hero
  in
    container w h middle <|
    collage areaW areaH
      [ toForm (image areaW areaH "/imgs/desert.png")
         |> move (0, offset)
      , toForm (image areaW areaH "/imgs/desert.png")
         |> move (0, areaH + offset)
      , (toForm (image 22 28 src))
          |> rotate (degrees (vx * -5))
          |> move (x, -areaH/2 + 28)
         -- , toForm <| show <| "y: " ++ toString (round y)
      -- , toForm (Markdown.toElement "Arrows to move<br/>Shift to run")
      --     |> move (70-areaW/2, 30-areaH/2)
      ]


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

