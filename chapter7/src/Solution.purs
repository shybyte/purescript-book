module Solution where

import Data.AddressBook.Validation (Errors, validateAddress, validatePhoneNumber, arrayNonEmpty, nonEmpty, matches)
import Data.Validation (V)
import Data.Foldable (class Foldable, foldr, foldl)
import Data.Monoid (mempty)
import Data.Maybe (Maybe)
import Data.Traversable (class Traversable, traverse, sequence)
import Data.String.Regex as R
import Control.Apply ((*>))
import Data.AddressBook (Address(Address), Person(Person), PhoneType(CellPhone, HomePhone), address, person, phoneNumber, PhoneNumber)
import Prelude
import Data.Either (Either(Right, Left))


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


exampleAddress :: Address
exampleAddress = address "Street" " " "12"



-- 7.11 Traversable Functors

-- 1. Exercise
-- Write a Traversable instance for the following binary tree data structure,
-- which combines side-effects from left-to-right:

data Tree a = Leaf | Branch (Tree a) a (Tree a)

instance showTree :: (Show a) => Show (Tree a) where
  show Leaf = "Leaf"
  show (Branch treeL a treeR) = "Branch (" ++ show treeL ++") "++ show a ++ " (" ++ show treeR ++ ")"

instance foldableTree :: Foldable Tree where
  foldr abb b Leaf  = b
  foldr abb b (Branch treeL a treeR)  = foldr abb (abb a (foldr abb b treeR)) treeL

  foldl bab b Leaf  = b
  foldl bab b (Branch treeL a treeR) = foldl bab (bab (foldl bab b treeL) a) treeR

  foldMap f xs = foldr (\x acc -> f x <> acc) mempty xs


instance functorTree :: Functor Tree where
  map = mapTree

mapTree :: forall a b. (a -> b) -> Tree a -> Tree b
mapTree fn (Branch treeL a treeR)  = Branch (mapTree fn treeL) (fn a) (mapTree fn treeR)
mapTree fn Leaf = Leaf


--class (Functor t, Foldable t) <= Traversable t where
--  traverse :: forall a b f. (Applicative f) => (a -> f b) -> t a -> f (t b)
--  sequence :: forall a f. (Applicative f) => t (f a) -> f (t a)
-- http://stackoverflow.com/questions/7460809/can-someone-explain-the-traverse-function-in-haskell
instance traversableTree :: Traversable Tree where
  traverse = traverseTree
  sequence = sequenceTree

traverseTree :: forall a b f. (Applicative f) => (a -> f b) -> Tree a -> f (Tree b)
traverseTree _ Leaf = pure Leaf
traverseTree aToMB (Branch treeL a treeR) = (Branch <$> (traverse aToMB treeL)) <*> aToMB a <*> traverse aToMB treeR

sequenceTree :: forall a f. (Applicative f) => Tree (f a) -> f (Tree a)
sequenceTree Leaf = pure Leaf
sequenceTree (Branch treeL a treeR)  = (Branch <$> (sequence treeL)) <*> a <*> (sequence treeR)

-- 3.Exercise
-- Try to write sequence in terms of traverse.
sequence2 :: forall a f t. (Applicative f, Traversable t) => t (f a) -> f (t a)
sequence2 tfa = traverse (\x -> x) tfa

-- Can you write traverse in terms of sequence?
traverse2 :: forall a b f t. (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
traverse2 a2fb ta =  sequence (a2fb <$> ta)

  -- 2. Exercise
  -- Modify the code to make the address field of the Person type optional using Data.Maybe.
  -- Hint: Use traverse to validate a field of type Maybe a.

newtype Person3 = Person3
  { firstName :: String
  , lastName  :: String
  , address   :: Maybe Address
  , phones    :: Array PhoneNumber
  }

person3 :: String -> String -> Maybe Address -> Array PhoneNumber -> Person3
person3 firstName lastName address phones = Person3
  { firstName: firstName
  , lastName:  lastName
  , address:   address
  , phones:    phones
  }

validatePerson3 :: Person3 -> V Errors Person3
validatePerson3 (Person3 o) =
  person3 <$> (nonEmpty "First Name" o.firstName *> pure o.firstName)
         <*> (nonEmpty "Last Name"  o.lastName  *> pure o.lastName)
         <*> addressValidator
         <*> (arrayNonEmpty "Phone Numbers" o.phones *> traverse validatePhoneNumber o.phones)
  where
    addressValidator :: V Errors (Maybe Address)
    addressValidator = validateMaybeAddress o.address

validateMaybeAddress :: (Maybe Address) -> V Errors (Maybe Address)
validateMaybeAddress maybeAddress = traverse validateAddress maybeAddress

instance showPerson3 :: Show Person3 where
  show (Person3 o) = "Person " ++
    "{ firstName: " ++ show o.firstName ++
    ", lastName: "  ++ show o.lastName ++
    ", address: "   ++ show o.address ++
    ", phones: "    ++ show o.phones ++
    " }"
