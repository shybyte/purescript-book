module Test.Main where

import Data.AddressBook
import Data.AddressBook.Validation

import Control.Monad.Eff.Console

import Prelude
import Solution
import Data.Maybe
import Data.Either


main = do
  print (validatePerson examplePerson)
  print $ ((+) 1) <$> Just 1
  print $ ((+) 1) <$> Nothing
  print $ (+) <$> Just 1 <*> Just 2
  print $ apply (map (+) (Just 1)) (Just 2)
  print $ (+) <$> Just 1 <*> Nothing
  print $ (+) <$> Nothing <*> Just 2
  print $ (+) <$> [1,2] <*> [10,20,30]
  print $ pure "123" :: Maybe String
  print $ fullName "Marco" "Shybyte" "Steel"
  print $ fullNameMaybe (Just "Marco") (Just "Shybyte") (Just "Steel")
  print $ fullNameEither (Just "Marco") (Just "Shybyte") (Just "Steel")
  print $ fullNameMaybe (Just "Marco") Nothing (Just "Steel")
  print $ fullNameEither (Just "Marco") Nothing (Just "Steel")
  print $ fullNameEither Nothing Nothing Nothing
