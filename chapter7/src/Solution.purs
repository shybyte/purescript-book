module Solution where

import Data.AddressBook.Validation
import Data.Validation
import Data.String.Regex as R
import Control.Apply ((*>))
import Data.AddressBook (Address(Address), Person(Person), PhoneType(CellPhone, HomePhone), address, person, phoneNumber)
import Data.Either (Either(Right, Left))
import Prelude ((++), Unit, ($), pure, (<*>), (<$>), unit)


examplePerson :: Person
examplePerson =
  person "John" "Smith"
         (address "123 Fake St." "FakeTown" "CA")
	     [ phoneNumber HomePhone "555-555-5555"
         , phoneNumber CellPhone "555-555-0000"
	     ]

nonEmpty2 :: String -> Either String Unit
nonEmpty2 "" = Left "Field cannot be empty"
nonEmpty2 _  = Right unit

validatePerson :: Person -> Either String Person
validatePerson (Person o) =
 person <$> (nonEmpty2 o.firstName *> pure o.firstName)
        <*> (nonEmpty2 o.lastName  *> pure o.lastName)
        <*> pure o.address
        <*> pure o.phones


fieldEmptyResult :: Either String Person
fieldEmptyResult = validatePerson $ person "" "" (address "" "" "") []
-- Left ("Field cannot be empty")


-- 7.10 Regular Expression Validators - Exercises

-- 1. Exercise
-- Use a regular expression validator to ensure
-- that the state field of the Address type contains two alphabetic characters.
-- Hint: see the source code for phoneNumberRegex.

stateRegex :: R.Regex
stateRegex =
  R.regex
    "^[a-z]{2}$"
    R.noFlags { ignoreCase = true}

validateState :: String -> V Errors String
validateState s = matches "State" stateRegex s *> pure s



validateAddress2 :: Address -> V Errors Address
validateAddress2 (Address o) =
  address <$> (nonBlank "Street" o.street *> pure o.street)
          <*> (nonBlank "City"   o.city   *> pure o.city)
          <*> validateState o.state

-- 2. Exercise
-- Using the matches validator, write a validation function which checks
-- that a string is not entirely whitespace.
-- Use it to replace nonEmpty where appropriate.

nonBlankRegex :: R.Regex
nonBlankRegex =
  R.regex
    "\\S"
    R.noFlags

nonBlank :: String -> String -> V Errors Unit
nonBlank field s  = matches field nonBlankRegex s


exampleAddress = address "Street" " " "12"
