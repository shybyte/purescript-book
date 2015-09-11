module Test.Main where

import Data.AddressBook
import Data.AddressBook.Validation

import Control.Monad.Eff.Console

import Prelude
import Solution
import Data.Maybe
import Data.Either
import Data.List

type EitherSN = Either String Int

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
  print $ combineList (toList [Just 1, Just 2, Just 3])
  print $ combineList (toList [Just 1, Just 2, Just 3])
  print $ combineList (toList [Left "text" :: Either String Number, Left "text2" :: Either String Number])
  print $ combineList (toList [Left "text" :: Either String Number, Right 123.0 :: Either String Number])
  print $ combineList (toList [Right 321 :: EitherSN, Right 123 :: EitherSN])
  print $ combineList (toList [Right 321.0 :: Either String Number, Left "text" :: Either String Number,  Right 123.0 :: Either String Number])
  print ((Just 1.0) +! (Just 2.0))
  print ((Just 1.0) +! (Nothing))
  print $ combineMaybe $ Just [1,2]
  print $ combineMaybe $ Nothing :: Maybe (Array Int)
  print $ combineMaybe $ Nothing :: Maybe (EitherSN)
  print $ combineMaybe $ Just (Left "test") :: Maybe (EitherSN)
  print $ combineMaybe $ Just (Right 123) :: Maybe (EitherSN)
