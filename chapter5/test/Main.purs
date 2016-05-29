module Test.Main where

import Control.Monad.Eff.Console
import Control.Monad.Eff (Eff)
import Prelude (Unit, negate, ($), bind, (<$>))

import Data.Picture


circle :: Shape
circle = Circle (Point { x: 0.0, y: 0.0 }) 10.0

rectangle :: Shape
rectangle = Rectangle (Point { x: 10.0, y: 10.0 }) 10.0 10.0

picture :: Picture
picture = [circle, rectangle]

clipped1 :: Shape
clipped1 = Clipped picture (Point { x: 0.0, y: 0.0 }) 5.0 5.0

clipped2 :: Shape
clipped2 = Clipped picture (Point { x: 0.0, y: 0.0 }) 22.0 22.0

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
  log (showBounds (bounds picture))
  log (showBounds (shapeBounds clipped1))
  log (showBounds (shapeBounds clipped2))
