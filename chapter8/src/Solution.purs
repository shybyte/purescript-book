module Solution where

import Prelude
import Data.Maybe
import Data.Array

-- 8.7.1
-- Look up the types of the head and tail functions from the Data.Array module in the purescript-arrays package.
-- Use do notation with the Maybe monad to combine these functions
-- into a function third which returns the third element of an array with three or more elements.
-- Your function should return an appropriate Maybe type.

third :: forall a. Array a -> Maybe a
third as = do
  tail1 <- tail as
  tail2 <- tail tail1
  head tail2


-- 8.7.1
-- Write a function sums which uses foldM to determine all possible totals that could be made using a set of coins.
-- The coins will be specified as an array which contains the value of each coin
-- Hint: This function can be written as a one-liner using foldM.
-- You might want to use the nub and sort functions to remove duplicates and sort the result respectively.
-- https://wiki.haskell.org/All_About_Monads

sums :: Array Int -> Array Int
sums =
  foldM (\a b -> [a, a+b, b]) 0
  >>> nub
  >>> sort


-- 8.7.3
-- Confirm that the ap function and the apply operator agree for the Maybe monad

-- class (Functor f) <= Apply f where
--   apply :: forall a b. f (a -> b) -> f a -> f b
--
-- instance applyMaybe :: Apply Maybe where
--   apply (Just fn) x = fn <$> x
--   apply Nothing   _ = Nothing
--
--
-- ap :: forall m. (Monad m) => m (a -> b) -> m a -> m b
-- ap mf ma = do
--  f <- mf -- "early return" if mf is Nothing
--  a <- ma -- "early return" if ma is Nothing
--  return (f a) -- f <$> ma == map f ma


-- 8.7.3 Verify that the monad laws hold for the Monad instance for the Maybe type, as defined in the purescript-maybe package.


-- class (Functor f) <= Apply f where
--   apply :: forall a b. f (a -> b) -> f a -> f b
--
-- class (Apply f) <= Applicative f where
--   pure :: forall a. a -> f a
--
-- class (Apply m) <= Bind m where
--   bind :: forall a b. m a -> (a -> m b) -> m b
--
-- class (Applicative m, Bind m) <= Monad m
--
-- instance applicativeMaybe :: Applicative Maybe where
--   pure = Just
--
-- instance bindMaybe :: Bind Maybe where
--   bind (Just x) k = k x
--   bind Nothing  _ = Nothing
--
-- -- Right identity law
--
-- rightIdentity monad = do -- == monad
--   x <- monad
--   return x
--
-- rightIdentity2 Nothing = Nothing
-- rightIdentity2 Just a = Just a
--
-- -- Left identity law
--
-- leftIdentity next = do  -- == next
--   x <- return y
--   next
--
-- leftIdentity2 next = bind (Just a) (\x -> next) -- = next
--
--
-- -- Associativity law.
--
-- c1 = do
--   y <- do
--     x <- m1
--     m2
--   m3
-- c1' = bind (bind m1 m2) m3
--
-- c2 = do
--   x <- m1
--   y <- m2
--   m3
-- c2' = bind m1 (bind m2 m3)
--
-- -- c1 == c2
--
-- c3 = do
--   x <- m1
--   do
--     y <- m2
--     m3
--
-- c3' = bind m1 (bind m2  m3)
--
-- -- c1 == c2 == c3


