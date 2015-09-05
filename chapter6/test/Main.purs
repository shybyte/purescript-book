module Test.Main where

import Prelude
import Data.Hashable
import Control.Monad.Eff.Console
import Data.Foldable
import Data.Array (length)

import Solution

--instance unsafeDefault :: Unsafe

--instance unsafe :: Unsafe

--unsafeLast :: forall a. (Unsafe) => Array a -> a
--unsafeLast array = unsafeIndex2 array (length array - 1)

main = do
  print (hash 123)
  print (hash true)
  print (hash [1, 2, 3])
  print (hash "testing")
  print (hash 'a')

  print ("foo" `hashEqual` "foo")
  print ("foo" `hashEqual` "bar")

  print (Complex {real: 5.0, imaginary: 2.0})
  print (NonEmpty 1 [2,3,4])
  print $ ((+) 1) <$> (NonEmpty 1 [2,3,4])
  print $ ((+) 1) <$> (NonEmpty 1 [2,3,4])
  print $ (NonEmpty 1 [2,3,4]) <> (NonEmpty 5 [6,7])
  print $ foldMap (show <<< ((+) 1) ) (NonEmpty 1 [2,3,4])
  print $ foldMap (show <<< ((+) 1) ) [1,2,3,4]
  print $ foldl (\b a -> b ++ show a) "Array: " [1,2,3,4]
  print $ foldl (\b a -> b ++ show a) "Array: " (NonEmpty 1 [2,3,4])
  print $ (NonEmpty 1 [2,3,4]) == (NonEmpty 1 [6,7])
  print $ (NonEmpty 1 [5]) == (NonEmpty 2 [5])
  print $ (NonEmpty 1 [2,3,4]) == (NonEmpty 1 [2,3,4])
  print $ (Finite 1) == (Finite 1)
  print $ (Finite 1) == (Finite 2)
  print $ (Finite 1) == Infinite
  print $ Infinite == Infinite :: Extended String
  print $ (Finite 1) `compare` (Finite 1)
  print $ (Finite 1) `compare` (Finite 2)
  print $ (Finite 1) `compare` Infinite
  print $ Infinite `compare` (Finite 1)
  print $ Infinite `compare` Infinite :: Extended String
  print $ foldMap (show <<< ((+) 1) ) (OneMore 1 [2,3,4])
  print $ act "0" (Self "123")
--  print $ unsafeLast [1,2,3]
  print $ hasDuplicates [1,2,3,2]
  print $ hasDuplicates ["1", "1"]
  print $ hasDuplicates [1,2,3]
