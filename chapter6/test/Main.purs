module Test.Main where

import Prelude
import Data.Hashable
import Control.Monad.Eff.Console
import Data.Foldable

import Solution

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
