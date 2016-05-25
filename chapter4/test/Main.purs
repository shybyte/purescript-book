module Test.Main where

import Control.Monad.Eff.Console
import Control.Monad.Eff (Eff)
import Data.Foldable (for_)
import Data.Path (root)
import FileOperations (countEven, allFiles, isEven)
import Prelude (Unit, bind, ($), (<$>))


testIsEven :: forall eff. Eff (console :: CONSOLE | eff) Unit
testIsEven = do
  print $ isEven <$> [0, 1, 2, 3]

testCountEven :: forall eff. Eff (console :: CONSOLE | eff) Unit
testCountEven = do
    print $ countEven [7]
    print $ countEven [0,1,3]

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
  for_ (allFiles root) print
  testIsEven
  testCountEven
