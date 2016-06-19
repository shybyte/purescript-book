module Example.Plot where

import Prelude ((*), (+), (-), bind, (/), (>>>), (<$>), unit, return, ($))
import Data.Int (toNumber)
import Data.Array ((..))
import Data.Maybe.Unsafe (fromJust)
import Control.Monad.Eff (Eff, foreachE)
import Graphics.Canvas
import Math (sin)


type Point = {x:: Number, y::Number}


renderPath :: forall eff. Context2D -> Array Point -> Eff (canvas :: Canvas | eff) Context2D
renderPath ctx points = do
  strokePath ctx $ do
    foreachE points $ \point -> do
      lineTo ctx point.x point.y
      return unit
    closePath ctx


plot :: forall eff. Context2D -> (Number -> Point) -> Eff (canvas :: Canvas | eff) Context2D
plot ctx f = do
  let points = (toNumber >>> (\x -> x / 100.0) >>> f) <$> (0 .. 100)
  renderPath ctx points

rotateAroundPoint :: forall eff. Context2D -> Number -> Point -> Eff (canvas :: Canvas | eff) Context2D
rotateAroundPoint ctx a center = do
  translate { translateX: center.x, translateY:  center.y } ctx
  rotate a ctx
  translate { translateX: 0.0-center.x, translateY:  0.0 - center.y } ctx


main :: forall eff. Eff (canvas:: Canvas | eff) Context2D
main = do
  canvasMaybe <- getCanvasElementById "canvas"
  let canvas = fromJust canvasMaybe
  ctx <- getContext2D canvas

  rotateAroundPoint ctx 0.8 {x: 300.0, y: 300.0}

  setStrokeStyle "#FF0000" ctx
  renderPath ctx  [{x: 10.0,y: 10.0}, {x: 50.0,y: 50.0}, {x: 100.0,y: 150.0}]
  renderPath ctx  [{x: 150.0,y: 10.0}, {x: 200.0,y: 50.0}, {x: 250.0,y: 150.0}]

  setStrokeStyle "#0000ff" ctx
  plot ctx (\x -> {x: x * 200.0, y: 200.0 + sin(x * 16.0) * 100.0})
