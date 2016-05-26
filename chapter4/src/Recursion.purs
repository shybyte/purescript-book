module Recursion where

import Data.Array (null)
import Data.Array.Unsafe (tail, head)
import Prelude ((+), (-))

isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven x = isEven (x - 2)


length :: forall a. Array a -> Int
length arr =
  if null arr
  then 0
  else 1 + length (tail arr)

countEven :: Array Int -> Int
countEven arr =
  if null arr
  then 0
  else
    length (tail arr) + (if isEven (head arr) then 0 else 1)
