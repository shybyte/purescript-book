module Test.Main where

import Control.Monad.Eff.Console
import Data.Path (root)
import FileOperations (allFiles)
import Control.Monad.Eff (Eff)
import Data.Foldable (for_)
import Prelude (Unit)

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = for_ (allFiles root) print
