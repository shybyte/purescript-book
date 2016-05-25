module FileOperations where

import Data.Array (null, (:), concatMap)
import Data.Array.Unsafe (tail, head)
import Data.Path (Path, ls)
import Prelude ((-), (+), bind)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child


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
