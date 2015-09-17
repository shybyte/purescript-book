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