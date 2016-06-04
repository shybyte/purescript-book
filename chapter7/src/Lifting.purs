module Lifting where

import Prelude
import Control.Apply
import Data.AddressBook
import Data.Maybe
import Data.Either
import Data.List
import Data.Array

-- 7.3 Generalizing Function Application
nothingResult = lift3 address (Just "123 Fake St.") (Just "Town") (Just "CA")
justResult = lift3 address (Just "123 Fake St.") Nothing (Just "CA")

--7.4 Lifting Arbitrary Functions
justResult2 = address <$> Just "123 Fake St." <*> Just "Faketown" <*> Just "CA"
nothingResult2= address  <$> Just "123 Fake St." <*> Nothing <*> Just "CA"

-- 7.7 More Effects
fullName :: String -> String -> String -> String
fullName first middle last = last ++ ", " ++ first ++ " " ++ middle

justFullName = fullName <$> Just "Phillip" <*> Just "A" <*> Just "Freeman"
nothingFullName = fullName <$> Just "Phillip" <*> Nothing <*> Just "Freeman"

toEither :: Maybe String -> String -> Either String String
toEither Nothing  err = Left err
toEither (Just a) _ = Right a

infix 9 toEither as <?>

fullNameEither :: Maybe String -> Maybe String -> Maybe String -> Either String String
fullNameEither first middle last =
    fullName <$> (first  <?> "First name was missing")
             <*> (middle <?> "Middle name was missing")
             <*> (last   <?> "Last name was missing")


right = fullNameEither (Just "Phillip") (Just "A") (Just "Freeman")
left = fullNameEither (Just "Phillip") Nothing (Just "Freeman")
leftAll = fullNameEither Nothing Nothing Nothing


-- 7.8 Combining Effects

combineList :: forall f a. (Applicative f) => List (f a) -> f (List a)
combineList Nil = pure Nil
combineList (Cons x xs) = Cons <$> x <*> combineList xs


justList = combineList (toList [Just 1, Just 2, Just 3])
nothingList = combineList  (toList [Just 1, Nothing, Just 2])


-- 7.8 Exercises

-- 1. Exercise
-- Use lift2 to write lifted versions of the numeric operators +, -, * and / which work with optional arguments.
optionalPlus :: forall t7 t8.( Apply t7, Semiring t8) => t7 t8 -> t7 t8 -> t7 t8
optionalPlus = lift2 (+)

infix 6 optionalPlus as ?+

justSum = (Just 2) ?+ (Just 3)
nothingSum = (Just 2) ?+ Nothing


-- 3. Exercise
-- Write a function combineMaybe which has type forall a f. (Applicative f) => Maybe (f a) -> f (Maybe a).
-- This function takes an optional computation with side-effects,
-- and returns a side-effecting computation which has an optional result.

combineMaybe :: forall f a. (Applicative f) => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just fa) =  Just <$> fa
-- combineList (Cons x xs) = Cons <$> x <*> combineList xs

arrayOfJust = combineMaybe $ Just [1,2,3]
-- [Just (1),Just (2),Just (3)]
nothingArray :: Maybe (Array Int)
nothingArray = Nothing
arrayOfNothing = combineMaybe nothingArray
-- [Nothing]
