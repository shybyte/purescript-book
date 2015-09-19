module Solution where

import Prelude
import Data.Maybe
import Data.List (List(..))
import Data.Array (foldM, nub, sort, tail, head)
import Control.Monad.Eff
import Control.Monad.Eff.Exception

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


-- 8.7.5
-- Write a function filterM which generalizes the filter function on lists.
-- Your function should have the following type signature:
-- Test your function in PSCi using the Maybe and Array monads.

filterM :: forall m a. (Monad m) => (a -> m Boolean) -> List a -> m (List a)
filterM p Nil     = return Nil
filterM p (Cons x xs) = do
  b  <- p x
  ys <- filterM p xs
  return (if b then (Cons x ys) else ys)



-- 8.7.6
-- Use the monad laws to prove that for any monad, the following holds:
-- lift2 f (return a) (return b) = return (f a b)

map2 :: forall m a b. (Monad m) => (a -> b) -> m a -> m b
map2 f ma = do
  a <- ma
  return (f a)

--  lift2 f (return a) (return b) = return (f a b)
--  lift2 f [a] [b] = [f a b]
--  lift2 f (Just 1) (Just 2) = Just (f 1 2)

-- lift2 :: forall f a b c. (Applicative f). (a -> b -> c) -> f a -> f b -> f c
-- lift2 f a b = f <$> a <*> b

lift2' :: forall m a b c. (Monad m) => (a -> b -> c) -> m a -> m b -> m c
lift2' fab_c ma mb = fab_c <$> ma <*> mb

lift2'2 :: forall m a b c. (Monad m) => (a -> b -> c) -> m a -> m b -> m c
lift2'2 fab_c ma mb = do
  -- map2 fab_c ma
  a <- ma
  let mfb_c = return (fab_c a)
  --ap mfb_c mb
  fb_c <- mfb_c
  b <- mb
  return (fb_c b)

lift2'3 :: forall m a b c. (Monad m) => (a -> b -> c) -> m a -> m b -> m c
lift2'3 fab_c ma mb = do
  a <- ma
  let fb_c = fab_c a
  b <- mb
  return (fb_c b)

lift2'4 :: forall m a b c. (Monad m) => (a -> b -> c) -> m a -> m b -> m c
lift2'4 fab_c ma mb = do
  a <- ma
  b <- mb
  return (fab_c a b)

-- 8.17.1
-- Rewrite the safeDivide function to throw an exception using throwException if the denominator is zero.

safeDivide :: forall eff. Int -> Int -> Eff (err :: EXCEPTION | eff) Int
safeDivide _ 0 = throwException $ error $ "Only Chuck Norris can divide by zero"
safeDivide a b = return (a / b)