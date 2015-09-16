module Test.Main where

import Control.Monad.Eff.Console

import Prelude
import Solution
import Data.Maybe


main = do
  -- 8.7.1
  print $ third ([] :: Array Int)
  print $ third [1]
  print $ third [1,2]
  print $ third [1,2,3]
  print $ third [1,2,3,4]
