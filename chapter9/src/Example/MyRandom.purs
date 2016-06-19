module Example.MyRandom where

import Prelude (Unit, unit, return, bind, ($), (*), (++), show, (>>>), (<$>))
import Data.String (joinWith)
import Data.Maybe
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, random)
import Graphics.Canvas (Context2D, Canvas, getContext2D, getCanvasElementById, arc, setStrokeStyle, setFillStyle, strokePath, fillPath)
import Control.Monad.Eff.DOM (addEventListener, querySelector)
import DOM (DOM)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Maybe.Unsafe (fromJust)
import Data.Traversable (for)


fillStrokePath :: forall eff a. Context2D -> Eff (canvas :: Canvas | eff) a -> Eff (canvas :: Canvas | eff) a
fillStrokePath ctx path = do
  fillPath ctx path
  strokePath ctx path


randomColor :: forall eff. Eff (random:: RANDOM | eff) String
randomColor = do
  r <- random
  g <- random
  b <- random
  let rgbArray = (((*) 100.0) >>> show >>> (\s -> s ++ "%")) <$> [r, g, b]
  return $ "rgb(" ++ (joinWith "," rgbArray) ++")"


drawRandomCircle :: forall eff. Context2D -> Eff (canvas :: Canvas, random::RANDOM | eff) Context2D
drawRandomCircle ctx = do
  fillColor <- randomColor
  strokeColor <- randomColor

  setFillStyle fillColor ctx
  setStrokeStyle strokeColor ctx

  x <- random
  y <- random
  r <- random

  let path = arc ctx
       { x     : x * 600.0
       , y     : y * 600.0
       , r     : r * 50.0
       , start : 0.0
       , end   : Math.pi * 2.0
       }

  fillStrokePath ctx path


main :: forall eff. Eff (canvas :: Canvas, console :: CONSOLE, dom :: DOM, random::RANDOM | eff) (Maybe Unit)
main = do
  canvasMaybe <- getCanvasElementById "canvas"
  let canvas = fromJust canvasMaybe
  ctx <- getContext2D canvas

  node <- querySelector "#canvas"
  for node $ addEventListener "click" $ do
    log "Mouse clicked!"
    drawRandomCircle ctx
    return unit
