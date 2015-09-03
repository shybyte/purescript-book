module Test.Main where

import Control.Monad.Eff.Console

import Prelude
import Data.Path
import Data.Foldable (for_)
import FileOperations

import Folds


testFolds = do
  print $ reverse [1,2,3]
  print $ reverser [1,2,3]

main = do
  for_ (allFiles root) print
  testFolds
