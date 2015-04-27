-- See this document for more information on making Pong:
-- http://elm-lang.org/blog/Pong.elm
-- Compile this code on http://elm-lang.org/try
 
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Mouse
import Text
import Time exposing (..)
import Window
 
-- SIGNALS
 
main =
--  Signal.map show input
  Signal.map2 view Window.dimensions gameState
 
gameState : Signal Game
gameState =
  Signal.foldp update defaultGame input
--}
delta =
  Signal.map inSeconds (fps 35)
--}
input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map4 Input
      Keyboard.space
      Mouse.y
      Window.height
      delta
 
 
-- MODEL
 
(gameWidth,gameHeight) = (600,400)
(halfWidth,halfHeight) = (300,200)
 
type State = Play | Pause
 
type alias Ball =
  { x:Float, y:Float, vx:Float, vy:Float }
 
type alias Player =
  { x:Float, y:Float, score:Int }
 
type alias Game =
  { state:State, ball:Ball, player1:Player, player2:Player }
 
 
player : Float -> Player
player x =
  { x = x, y = 0, score = 0 }
 
defaultGame : Game
defaultGame =
  { state   = Pause
  , ball    = { x=0, y=0, vx=200, vy=200 }
  , player1 = player (20-halfWidth) 
  , player2 = player (halfWidth-20)
  }
 
 
type alias Input =
    { space : Bool
    , mouseY : Int
    , windowH : Int
    , delta : Time
    }
 
 
-- UPDATE
 
update : Input -> Game -> Game
update {space,mouseY,windowH,delta} ({state,ball,player1,player2} as game) =
  let score1 = if ball.x >  halfWidth then 1 else 0
      score2 = if ball.x < -halfWidth then 1 else 0
 
      newState =
        if  | space            -> Play
            | score1 /= score2 -> Pause
            | otherwise        -> state
 
      newBall =
        if state == Pause
            then ball
            else updateBall delta ball player1 player2
 
  in
      { game |
          state <- newState,
          ball <- newBall,
          player1 <- updatePlayer mouseY windowH score1 player1,
          player2 <- updatePlayer mouseY windowH score2 player2
      }
 
 
updateBall : Time -> Ball -> Player -> Player -> Ball
updateBall t ({x,y,vx,vy} as ball) p1 p2 =
  if not (ball.x |> near 0 halfWidth)
    then { ball | x <- 0, y <- 0 }
    else physicsUpdate t
            { ball |
                vx <- stepV vx (ball `within` p1) (ball `within` p2),
                vy <- stepV vy (y < 7-halfHeight) (y > halfHeight-7)
            }
 
 
updatePlayer : Int -> Int -> Int -> Player -> Player
updatePlayer mouseY windowH points player =
  let player1 = { player | y <- toFloat windowH / 2 - toFloat mouseY }
  in
      { player1 |
          y <- clamp (22-halfHeight) (halfHeight-22) player1.y,
          score <- player.score + points
      }
 
 
physicsUpdate t ({x,y,vx,vy} as obj) =
  { obj |
      x <- x + vx * t,
      y <- y + vy * t
  }
 
 
near k c n =
    n >= k-c && n <= k+c
 
within ball paddle =
    near paddle.x 8 ball.x && near paddle.y 20 ball.y
 
 
stepV v lowerCollision upperCollision =
  if | lowerCollision -> abs v
     | upperCollision -> 0 - abs v
     | otherwise      -> v
 
 
-- VIEW
 
view : (Int,Int) -> Game -> Element
view (w,h) {state,ball,player1,player2} =
  let scores : Element
      scores = txt (Text.height 50) (toString player1.score ++ "  " ++ toString player2.score)
  in
      container w h middle <|
      collage gameWidth gameHeight
        [ rect gameWidth gameHeight
            |> filled pongGreen
        , oval 15 15
            |> make ball
        , rect 10 40
            |> make player1
        , rect 10 40
            |> make player2
        , toForm scores
            |> move (0, gameHeight/2 - 40)
        , toForm (if state == Play then spacer 1 1 else txt identity msg)
            |> move (0, 40 - gameHeight/2)
        ]
 
pongGreen = rgb 60 100 60
textGreen = rgb 160 200 160
txt f = Text.fromString >> Text.color textGreen >> Text.monospace >> f >> leftAligned
msg = "SPACE to start, mouse to move"
make obj shape =
    shape
      |> filled white
      |> move (obj.x,obj.y)
