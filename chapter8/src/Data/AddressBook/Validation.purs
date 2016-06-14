module Data.AddressBook.Validation where

import Data.Traversable
import Data.String as S
import Data.String.Regex as R
import Control.Apply ((*>))
import Data.AddressBook (PhoneType(OtherPhone, CellPhone, WorkPhone, HomePhone), Address(Address), Person(Person), PhoneNumber(PhoneNumber), person, phoneNumber, address)
import Data.Either (Either(Right, Left))
import Data.Validation (V, runV, invalid)
import Prelude (Unit, ($), (<*>), pure, (<$>), (++), unit, show, (/=), Show)


type Errors = Array ValidationError

data ValidationError = ValidationError String Field

data Field = FirstNameField
          | LastNameField
          | StreetField
          | CityField
          | StateField
          | StateField
          | PhoneFields
          | PhoneField PhoneType

instance showField :: Show Field where
  show FirstNameField = "First Name"
  show LastNameField = "Last Name"
  show StreetField = "Street"
  show CityField = "City"
  show StateField = "State"
  show PhoneFields = "Phones"
  show (PhoneField phoneType) = (localizePhoneType phoneType) ++ " Phone"


localizePhoneType :: PhoneType -> String
localizePhoneType phoneType =
  case phoneType of
    HomePhone -> "Home"
    WorkPhone -> "Work"
    CellPhone -> "Cell"
    OtherPhone -> "Other"


nonEmpty :: Field -> String -> V Errors Unit
nonEmpty field "" = invalid [ValidationError ("Field '" ++ show field ++ "' cannot be empty") field]
nonEmpty _     _  = pure unit

arrayNonEmpty :: forall a. Field -> Array a -> V Errors Unit
arrayNonEmpty field [] = invalid [ ValidationError ("Field '" ++ show field ++ "' must contain at least one value") field]
arrayNonEmpty _     _  = pure unit

lengthIs :: Field -> Int -> String -> V Errors Unit
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

matches :: Field -> R.Regex -> String -> V Errors Unit
matches _     regex value | R.test regex value = pure unit
matches field _     _     = invalid [ValidationError ("Field '" ++ show field ++ "' did not match the required format") field]

validateAddress :: Address -> V Errors Address
validateAddress (Address o) =
  address <$> (nonEmpty StreetField o.street *> pure o.street)
          <*> (nonEmpty CityField   o.city   *> pure o.city)
          <*> (lengthIs StateField 2 o.state *> pure o.state)

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber (PhoneNumber o) =
  phoneNumber <$> pure o."type"
              <*> (matches (PhoneField o.type) phoneNumberRegex o.number *> pure o.number)

validatePerson :: Person -> V Errors Person
validatePerson (Person o) =
  person <$> (nonEmpty FirstNameField o.firstName *> pure o.firstName)
         <*> (nonEmpty LastNameField  o.lastName  *> pure o.lastName)
         <*> validateAddress o.address
         <*> (arrayNonEmpty PhoneFields o.phones *> traverse validatePhoneNumber o.phones)

validatePerson' :: Person -> Either Errors Person
validatePerson' p = runV Left Right $ validatePerson p
