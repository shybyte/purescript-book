module Main where

import Control.Monad.Eff
import Prelude ((+), (*), Unit)
import Math (sqrt,pi)
import Control.Monad.Eff.Console (CONSOLE, print)
import Global (readFloat)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

circleArea :: Number -> Number
circleArea r = r * r * pi

circleAreaFromString :: String -> Number
circleAreaFromString rString = circleArea (readFloat rString)

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = print (diagonal 3.0 4.0)
