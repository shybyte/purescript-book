module Example.Rectangle where

import Prelude (bind, ($), (*), (+))
import Data.Maybe.Unsafe (fromJust)
import Control.Monad.Eff (Eff)
import Graphics.Canvas

translate :: forall r. Number -> Number ->
              { x :: Number, y :: Number | r } ->
              { x :: Number, y :: Number | r }
translate dx dy shape = shape
  { x = shape.x + dx
  , y = shape.y + dy
  }


main :: forall eff. Eff (canvas:: Canvas | eff) Context2D
main = do
  canvasMaybe <- getCanvasElementById "canvas"
  let canvas = fromJust canvasMaybe
  ctx <- getContext2D canvas


  let myRect = { x: 250.0,
                 y: 250.0,
                 w: 100.0,
                 h: 100.0
               }

  -- Try changing the Rectangle example to render two rectangles side-by-side
  -- using the same call to fillPath .
  setFillStyle "#0000FF" ctx
  fillPath ctx $ do
    rect ctx myRect
    rect ctx $ translate 120.0 0.0 myRect

  -- Experiment with the strokePath and setStrokeStyle functions
  setStrokeStyle "#FF0000" ctx
  strokePath ctx $ rect ctx myRect

  --Try rendering a sector of a circle by using a combination of a piecewise-linear path and an arc segment.
  setFillStyle "#FF8800" ctx
  fillPath ctx $ do
    arc ctx
      { x: 100.0
      , y: 100.0
      , r: 50.0
      , start: 0.0
      , end: Math.pi * 2.0
      }
  setFillStyle "#ffffff" ctx
  fillPath ctx $ do
    moveTo ctx 100.0 100.0
    lineTo ctx 150.0 150.0
    lineTo ctx 100.0 150.0
    closePath ctx
