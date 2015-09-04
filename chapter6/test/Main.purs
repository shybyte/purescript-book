module Test.Main where

import Prelude
import Data.Hashable
import Control.Monad.Eff.Console

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
