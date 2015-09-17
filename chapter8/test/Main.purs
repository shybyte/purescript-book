module Test.Main where

import Control.Monad.Eff.Console

import Prelude
import Solution
import Data.Maybe
import qualified Data.Array as A
import qualified Data.List as L


main = do
  -- 8.7.1
  print $ third ([] :: Array Int)
  print $ third [1]
  print $ third [1,2]
  print $ third [1,2,3]
  print $ third [1,2,3,4]
  -- 8.7.2
  print $ sums [] -- [0]
  print $ sums [1, 2, 10] -- [0,1,2,3,10,11,12,13]
  --8.7.5
  print $ filterM (\a -> Just (a>6)) (L.toList [1,3,10,11])
  print $ filterM (\a -> if a == 3 then Nothing else Just (a>0)) (L.toList [1,3,10,11])
  print $ filterM (\a -> [true,false,true]) (L.toList [1,2])
  print $ A.filterM (\a -> [true,false])  [1,2,3]