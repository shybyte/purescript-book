module Arrays where

import Control.MonadPlus (guard)
import Data.Array (concat, length, (..), filter, concatMap)
import Data.Array.Unsafe (head)
import Data.Foldable (product)
import Prelude ((++), return, (==), (*), (/), (+), (-), ($), bind, map, (>),  (>=), (<$>), mod)

squares :: Array Number -> Array Number
squares xs = (\x -> x * x) <$> xs

removeNegative :: Array Number -> Array Number
removeNegative = filter (\x -> x > 0.0)

infix 4 filter as $?

removeNegative2 :: Array Number -> Array Number
removeNegative2 xs = (\x -> x > 0.0) $? xs


-- 4.9 Array Comprehensions

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


-- 4.11 Exercises


isPrime :: Int -> Boolean
isPrime 1 = false
isPrime x = (length (factors x)) == 1

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do
  x <- xs
  y <- ys
  return [x, y]

pythTriple :: Int -> Array (Array Int)
pythTriple n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ a * a + b *b  == c * c
  return [a, b, c]

factorize :: Int -> Array (Array Int)
factorize 1 = []
factorize x = do
  factor <- findFactors x
  let factor2 = x / factor
  if factor2 >= factor
    then [[factor, factor2]] ++
         map (\fs -> [factor] ++ fs)
           (filter (\fs2 -> head fs2 >= factor ) (factorize factor2))
    else []

findFactors :: Int -> Array (Int)
findFactors 1 = []
findFactors 2 = []
findFactors x = filter (\f -> x `mod` f == 0) (2 .. (x - 1))
