module FileOperations where

import Control.MonadPlus (guard)
import Data.Array (filter, null, (:), concatMap, (..))
import Data.Array.Unsafe (tail, head)
import Data.Foldable (product)
import Data.Path (Path, ls)
import Prelude (return, ($), (>), (*), (<$>), (+), (-), (==), bind, map)
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


squares :: Array Number -> Array Number
squares xs = (\x -> x * x) <$> xs

removeNegative :: Array Number -> Array Number
removeNegative = filter (\x -> x > 0.0)


infix 4 filter as $?

removeNegative2 :: Array Number -> Array Number
removeNegative2 xs = (\x -> x > 0.0) $? xs

pairs :: Int -> Array (Array Int)
pairs n = concatMap (\i -> map (\j -> [i, j]) (i .. n)) (1 .. n)

factors :: Int -> Array (Array Int)
factors n = filter (\pair -> product pair == n) (pairs n)


factorsDo :: Int -> Array (Array Int)
factorsDo n = filter (\xs -> product xs == n) $ do
  i <- 1 .. n
  j <- i .. n
  return [i, j]

factorsDoGuard :: Int -> Array (Array Int)
factorsDoGuard n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  return [i, j]
