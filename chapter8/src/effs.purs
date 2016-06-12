module Effs where

import Prelude (Unit, (/), (*), return, ($), bind, unit, (+), (<), (-))
import Control.Monad.Eff (Eff, forE, runPure)
import Control.Monad.Eff.Random (RANDOM, random)
import Control.Monad.Eff.Console (CONSOLE, print)
import Control.Monad.ST (ST, readSTRef, modifySTRef, newSTRef, runST)
import Control.Monad.Eff.Exception (EXCEPTION, throwException, error)

main :: forall eff h. Eff (console :: CONSOLE, random :: RANDOM, st :: ST h | eff) Unit
main = do
  n <- random
  print n
  s <- simulate 1.0 1.0 1.0
  print s

simulate :: forall eff h. Number -> Number -> Number -> Eff (st :: ST h | eff) Number
simulate x0 v0 time = do
  ref <- newSTRef { x: x0, v: v0 }
  forE 0.0 (time * 1000.0) $ \i -> do
    modifySTRef ref (\o ->
      { v: o.v - 9.81 * 0.001
      , x: o.x + o.v * 0.001
      })
    return unit
  final <- readSTRef ref
  return final.x

simulate' :: Number -> Number -> Number -> Number
simulate' x0 v0 time = runPure (runST (simulate x0 v0 time))


-- 8.17 Mutable State

-- 1. Exercise
-- Rewrite the safeDivide function to throw an exception using throwException
-- if the denominator is zero.


safeDivide :: forall eff. Int -> Int -> Eff (err :: EXCEPTION | eff) Int
safeDivide _ 0 = throwException $ error "Expected a non-negative number"
safeDivide a b = return $ a / b


-- 2. Exercise
-- The following is a simple way to estimate pi:
-- randomly choose a large number N of points in the unit square,
-- and count the number n which lie in the inscribed circle.
-- An estimate for pi is 4n/N.
-- Use the RANDOM and ST effects with the forE function
-- to write a function which estimates pi in this way.


estimatePi :: forall eff h. Number  -> Eff (st :: ST h, random :: RANDOM | eff) Number
estimatePi n = do
  ref <- newSTRef { count: 0.0}
  forE 0.0 n $ \i -> do
    x <- random
    y <- random
    modifySTRef ref (\o ->
      if x*x + y*y < 1.0
      then { count: o.count + 1.0 }
      else o
    )
    return unit
  final <- readSTRef ref
  return $ 4.0 * final.count / n
