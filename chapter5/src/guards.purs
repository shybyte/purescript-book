module Guards where

import Data.Foldable
import Prelude (otherwise, (==), (+), (-), (*))


-- 1. Exercise
-- Write the factorial function using pattern matching.
-- Hint: Consider the two cases zero and non-zero inputs.

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)


-- 2. Exercise
-- Look up Pascal’s Rule for computing binomial coefficients.
-- Use it to write a function which computes binomial coefficients using pattern matching.
binomial :: Int -> Int -> Int
binomial n 0 = 1
binomial n k
  | n == k    = 1
  | otherwise = binomial (n - 1) (k - 1) +  binomial (n - 1) k
