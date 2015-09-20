module Data.AddressBook.Validation where

import Prelude

import Data.Array
import Data.Either
import Data.Validation
import Data.AddressBook
import Data.Traversable

import qualified Data.String as S
import qualified Data.String.Regex as R

import Control.Apply

data ValidationError = ValidationError String Field
type ValidationErrors = Array ValidationError

data Field = FirstNameField
           | LastNameField
           | StreetField
           | CityField
           | StateField
           | PhoneFields
           | PhoneField PhoneType

instance showBoolean :: Show Field where
  show FirstNameField = "FirstName"
  show LastNameField = "LastName"
  show StreetField = "Street"
  show CityField = "City"
  show StateField = "State"
  show PhoneFields = "Phones"
  show (PhoneField phoneType) = show phoneType


nonEmpty :: Field -> String -> V ValidationErrors Unit
nonEmpty field "" = invalid [ValidationError ("Field '" ++ show field ++ "' cannot be empty" ) field]
nonEmpty _     _  = pure unit

arrayNonEmpty :: forall a. Field -> Array a -> V ValidationErrors Unit
arrayNonEmpty field [] = invalid [ValidationError ("Field '" ++ show field ++ "' must contain at least one value") field]
arrayNonEmpty _     _  = pure unit

lengthIs :: Field -> Int -> String -> V ValidationErrors Unit
lengthIs field len value | S.length value /= len = invalid [ValidationError ("Field '" ++ show field ++ "' must have length " ++ show len) field]
lengthIs _     _   _     = pure unit

phoneNumberRegex :: R.Regex
phoneNumberRegex = 
  R.regex 
    "^\\d{3}-\\d{3}-\\d{4}$" 
    { unicode:    false
    , sticky:     false
    , multiline:  false
    , ignoreCase: false
    , global:     false 
    }

matches :: Field -> R.Regex -> String -> V ValidationErrors Unit
matches _     regex value | R.test regex value = pure unit
matches field _     _     = invalid [ValidationError ("Field '" ++ show field ++ "' did not match the required format") field]

validateAddress :: Address -> V ValidationErrors Address
validateAddress (Address o) = 
  address <$> (nonEmpty StreetField o.street *> pure o.street)
          <*> (nonEmpty CityField   o.city   *> pure o.city)
          <*> (lengthIs StateField 2 o.state *> pure o.state)

validatePhoneNumber :: PhoneNumber -> V ValidationErrors PhoneNumber
validatePhoneNumber (PhoneNumber o) = 
  phoneNumber <$> pure o."type"
              <*> (matches (PhoneField o.type) phoneNumberRegex o.number *> pure o.number)

validatePerson :: Person -> V ValidationErrors Person
validatePerson (Person o) =
  person <$> (nonEmpty FirstNameField o.firstName *> pure o.firstName)
         <*> (nonEmpty LastNameField  o.lastName  *> pure o.lastName)
         <*> validateAddress o.address
         <*> (arrayNonEmpty PhoneFields o.phones *> traverse validatePhoneNumber o.phones)

validatePerson' :: Person -> Either ValidationErrors Person
validatePerson' p = runV Left Right $ validatePerson p
