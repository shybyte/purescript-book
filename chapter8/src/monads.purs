module Monads where

import Prelude (class Monad, Unit, return, ($), bind, (>>>), (+), (/), (==))
import DOM (DOM)
import Data.Maybe (Maybe(Just, Nothing))
import Control.Plus (empty)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.AddressBook.UI (setupEventHandlers)
import Data.Array (sort, (..), tail, head, foldM, nub)
import Data.List (List(Cons, Nil), toList)

main :: forall eff. Eff (console :: CONSOLE, dom :: DOM | eff) Unit
main = do
  log "Attaching event handlers"
  setupEventHandlers


countThrows :: Int -> Array (Array Int)
countThrows n = do
  x <- 1 .. 6
  y <- 1 .. 6
  if x + y == n
    then return [x, y]
    else empty


foldM' :: forall m a b. (Monad m) => (a -> b -> m a) -> a -> List b -> m a
foldM' _ a Nil = return a
foldM' f a (Cons b bs) = do
  a' <- f a b
  foldM' f a' bs


safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide a b = Just (a / b)

just5 :: Maybe Int
just5 = foldM' safeDivide 100 (toList [5, 2, 2])

nothing :: Maybe Int
nothing = foldM' safeDivide 100 (toList [2, 0, 4])



-- 8.7 Monads and Applicatives

-- 1. Exercise

third :: forall a. Array a -> Maybe a
third as = do
  tail1 <- tail as
  tail2 <- tail tail1
  head3 <- head tail2
  return head3


-- 2. Exercise
-- Write a function sums which uses foldM to determine all possible totals
-- that could be made using a set of coins.
-- Hint: This function can be written as a one-liner using foldM.
-- You might want to use the nub and sort functions
-- to remove duplicates and sort the result respectively.

-- foldM :: forall m a b. (Monad m) => (a -> b -> m a) -> a -> List b -> m a
sums :: Array Int -> Array Int
sums = foldM ab2ma 0 >>> sort >>> nub
  where
  ab2ma a b = [a, b, a + b]


-- 5. Exercise
-- Write a function filterM which generalizes the filter function on lists.

filterM' :: forall m a. (Monad m) => (a -> m Boolean) -> List a -> m (List a)
filterM' _ Nil = return Nil
filterM' f (Cons b bs) = do
  bool <- f b
  t <- filterM' f bs
  if bool
    then return $ Cons b t
    else return t

-- filterM (\a -> if a < 10 then Just(true) else Nothing) ([1,2])
-- -> Just ([1,2])

-- filterM (\a -> if a < 10 then Just(true) else Nothing) ([1,2, 10])
-- -> Nothing

-- filterM (\a -> Just(a < 10)) ([1,2, 10])
-- -> Just ([1,2])

-- filterM (\a -> [true,false]) ([1,2,3])
-- -> [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]

-- filterM (\a -> [a<10]) ([1,2,10])
-- -> [[1,2]]
