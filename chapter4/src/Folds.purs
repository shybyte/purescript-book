module Folds where

import Control.Apply ((<*))
import Control.MonadPlus (guard)
import Data.Array ((..), filter, length, concatMap)
import Data.Array.Unsafe (tail, head)
import Data.Foldable (foldr, product, foldl)
import Prelude ((&&), (++), return, (==), (*), (/), (+), (-), ($), bind, map, (>), (>=), (<$>), mod)


-- 4.15 1 Use foldl to test whether an array of boolean values are all true.
allTrue :: Array Boolean -> Boolean
allTrue = foldl (\acc x -> acc && x) true

-- 4.15 2
oddFalseCount :: Array Boolean -> Boolean
oddFalseCount = foldl (==) false

-- 4.15 3 Rewrite the following function in tail recursive form
count :: forall a. (a -> Boolean) -> Array a -> Int
count _ [] = 0
count p xs = if p (head xs)
             then count p (tail xs) + 1
             else count p (tail xs)

countTr :: forall a. (a -> Boolean) -> Array a -> Int
countTr _ [] = 0
countTr p xs = countTr' 0 p xs
  where
  countTr' acc _ []  = acc
  countTr' acc p xs =
    if p (head xs)
    then countTr' (acc + 1) p (tail xs)
    else countTr' acc p (tail xs)

-- 4.15 4 Write reverse in terms of foldl.
reverse :: forall a. Array a -> Array a
reverse = foldr (\x xs -> xs ++ [x]) []

reverseFoldL :: forall a. Array a -> Array a
reverseFoldL = foldl (\acc x -> [x] ++ acc) []
