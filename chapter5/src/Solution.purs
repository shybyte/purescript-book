module Solution where

import Prelude
import Data.Picture


factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n-1)


binomialCoefficients :: Int -> Int -> Int
binomialCoefficients n 0 = 1
binomialCoefficients n k
  | n==k = 1
  | otherwise = (binomialCoefficients (n-1) (k-1)) + (binomialCoefficients (n-1) k)
