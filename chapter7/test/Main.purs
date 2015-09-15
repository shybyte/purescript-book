module Test.Main where

import Data.AddressBook
import Data.AddressBook.Validation

import Control.Monad.Eff.Console

import Prelude
import Solution
import Data.Maybe
import Data.Either
import Data.List
import Data.Traversable

type EitherSN = Either String Int

tree123 = Branch (Branch Leaf 1 Leaf) 2 (Branch Leaf 3 Leaf)
treeMaybe1 = Branch Leaf (Just 1) Leaf
treeMaybeNothing = Branch Leaf (Just 1) (Branch Leaf Nothing  Leaf)

main = do
  print (validatePerson examplePerson)
  print $ tree123
  print $ traverse Just tree123
  print $ traverse (\x -> Just (show x)) tree123
  print $ traverse (\x -> if x == 3 then Nothing else Just (show x)) tree123
  print $ sequence treeMaybe1
  print $ sequence treeMaybeNothing
  print $ validatePhoneNumber $ phoneNumber HomePhone "555-555-5555"
  print $ validatePhoneNumber $ phoneNumber HomePhone "555.555-5555"
  print $ validateAddress2 $ address "" "" ""
  print $ validateAddress2 $ address "1" "2" "22"
  print $ validateAddress2 $ address "Auer" "Berlin" "CA"
--  print $ ((+) 1) <$> Just 1
--  print $ ((+) 1) <$> Nothing
--  print $ (+) <$> Just 1 <*> Just 2
--  print $ apply (map (+) (Just 1)) (Just 2)
--  print $ (+) <$> Just 1 <*> Nothing
--  print $ (+) <$> Nothing <*> Just 2
--  print $ (+) <$> [1,2] <*> [10,20,30]
--  print $ pure "123" :: Maybe String
--  print $ fullName "Marco" "Shybyte" "Steel"
--  print $ fullNameMaybe (Just "Marco") (Just "Shybyte") (Just "Steel")
--  print $ fullNameEither (Just "Marco") (Just "Shybyte") (Just "Steel")
--  print $ fullNameMaybe (Just "Marco") Nothing (Just "Steel")
--  print $ fullNameEither (Just "Marco") Nothing (Just "Steel")
--  print $ fullNameEither Nothing Nothing Nothing
--  print $ combineList (toList [Just 1, Just 2, Just 3])
--  print $ combineList (toList [Just 1, Nothing, Nothing])
--  print $ combineList (toList [Left "text" :: Either String Number, Left "text2" :: Either String Number])
--  print $ combineList (toList [Left "text" :: Either String Number, Right 123.0 :: Either String Number])
--  print $ combineList (toList [Right 321 :: EitherSN, Right 123 :: EitherSN])
--  print $ combineList (toList [Right 321.0 :: Either String Number, Left "text" :: Either String Number,  Right 123.0 :: Either String Number])
--  print ((Just 1.0) +! (Just 2.0))
--  print ((Just 1.0) +! (Nothing))
--  print $ combineMaybe $ Just [1,2]
--  print $ combineMaybe $ Nothing :: Maybe (Array Int)
--  print $ combineMaybe $ Nothing :: Maybe (EitherSN)
--  print $ combineMaybe $ Just (Left "test") :: Maybe (EitherSN)
--  print $ combineMaybe $ Just (Right 123) :: Maybe (EitherSN)
--  print $ validatePerson2 $ person "" "" (address "" "" "") []
--  print $ validatePerson2 $ person "Marco" "Steel" (address "Here" "and" "there") []
--  print $ validateAddress $ address "" "" ""
--  print $ validateAddress $ address "Auer" "Berlin" "CA"
--  print $ traverse (nonEmpty "Example") Nothing
--  print $ traverse (nonEmpty "Example") (Just "")
--  print $ traverse (nonEmpty "Example") (Just "Testing")
