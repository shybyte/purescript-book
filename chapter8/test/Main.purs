module Test.Main where

import Control.Monad.Eff.Console (print)

import Prelude
import Solution
import Data.Maybe
import Control.Apply (lift2)
import qualified Data.Array as A
import qualified Data.List as L
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Debug.Trace (trace)


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
  -- 8.7.5
  print $ filterM (\a -> Just (a>6)) (L.toList [1,3,10,11])
  print $ filterM (\a -> if a == 3 then Nothing else Just (a>0)) (L.toList [1,3,10,11])
  print $ filterM (\a -> [true,false,true]) (L.toList [1,2])
  print $ A.filterM (\a -> [true,false])  [1,2,3]
  -- 8.7.6
  print $ map2 ((+) 1) [1,2,3]
  print $ lift2 (+) [1] [2]
  print $ lift2'4 (+) [1] [2]
  print $ lift2 (+) Nothing (Just 2)
  print $ lift2 (+) (Just 1) (Just 2)
  -- 8.17.1
  -- result <- safeDivide 10 5
  -- print $ result
  -- print "This was OK."
  -- this would throw a runtime exception
  --  error <- safeDivide 10 0
  --  print $ error
  --  print "This was NOT OK."
  valueOrDefault <- catchException printExceptionReturnDefault $ safeDivide 10 0
  print valueOrDefault

  where
  printExceptionReturnDefault :: forall eff. Error -> Eff eff Int
  printExceptionReturnDefault e = trace (message e) (\_ -> return 23)
