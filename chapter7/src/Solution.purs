module Solution where

import Prelude
import Data.Maybe
import Data.Either
import Data.List
import Data.Traversable
import Data.Foldable
import Data.Monoid
import Data.Validation
import qualified Data.String.Regex as R

import Control.Apply ((*>))

import Data.AddressBook
import Data.AddressBook.Validation


--class Functor f where
--  map :: forall a b. (a -> b) ->
--                     f a -> f b
--
--class (Functor f) <= Apply f where
--  apply :: forall a b. f (a -> b) ->
--                       f a -> f b

--instance functorMaybe :: Functor Maybe where
--  map f (Just a) = Just (f a)
--  map f Nothing  = Nothing
--
--instance applyMaybe :: Apply Maybe where
--  apply (Just f) (Just x) = Just (f x)
--  apply _        _        = Nothing

lift1 :: forall a b f. (Prelude.Apply f) =>
                           (a -> b) ->
                           f a -> f b
--lift1 f x y z = f <$> x
lift1 f x = map f x


lift2 :: forall a b c f. (Prelude.Apply f) =>
                           (a -> (b -> c)) ->
                           f a -> f b -> f c
--lift2 f x y z = f <$> x <*> y
--lift2 f x y = apply (map  f x) y
lift2 fun a b = apply liftedfx b
  where
  liftedfx :: f (b -> c)
  liftedfx = map fun a


lift3 :: forall a b c d f. (Prelude.Apply f) =>
                           (a -> b -> c -> d) ->
                           f a -> f b -> f c -> f d
--lift3 f x y z = f <$> x <*> y <*> z
lift3 f x y z = apply (apply (map  f x) y) z


--class (Apply f) <= Applicative f where
--  pure :: forall a. a -> f a

--instance applicativeMaybe :: Applicative Maybe where
--  pure x = Just x

fullName first middle last = last ++ ", " ++ first ++ " " ++ middle

fullNameMaybe :: Maybe String -> Maybe String -> Maybe String -> Maybe String
fullNameMaybe first middle last = fullName <$> first <*> middle <*> last


(<?>) :: forall a e. Maybe a -> e  -> Either e a
(<?>) Nothing  err = Left err
(<?>) (Just a) _ = Right a


fullNameEither :: Maybe String -> Maybe String -> Maybe String -> Either String String
--fullNameEither first middle last =
--    fullName <$> (first  <?> "First name was missing")
--             <*> (middle <?> "Middle name was missing")
--             <*> (last   <?> "Last name was missing")
fullNameEither first middle last =
    lift3 fullName (first  <?> "First name was missing") (middle <?> "Middle name was missing") (last   <?> "Last name was missing")

--class (Functor f) <= Apply f where
--  apply :: forall a b. f (a -> b) -> f a -> f b
-- Cons :: a -> (List a -> List a)

combineList :: forall f a. (Applicative f) => List (f a) -> f (List a)
combineList Nil = pure Nil
--combineList (Cons x xs) = Cons <$> x <*> combineList xs
combineList (Cons x xs) = apply mappedHead combinedTail
  where
    mappedHead :: f (List a -> List a)
    mappedHead = (map Cons x)
    combinedTail :: f (List a)
    combinedTail = combineList xs



(+!) :: forall f. (Applicative f) => f Number -> f Number -> f Number
(+!) = lift2 (+)


--class Functor f where
--  map :: forall a b. (a -> b) ->
--                     f a -> f b
--

combineMaybe :: forall a f. (Applicative f) => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just fa) = map Just fa

--mapFA :: (a -> Maybe a) -> f a -> f (Maybe a)


nonEmpty2 :: String -> Either String Unit
nonEmpty2 "" = Left "Field cannot be empty"
nonEmpty2 _  = Right unit

validatePerson2 :: Person -> Either String Person
--validatePerson2 (Person o) =
--  person <$> (nonEmpty2 o.firstName *> pure o.firstName)
--         <*> (nonEmpty2 o.lastName  *> pure o.lastName)
--         <*> pure o.address
--         <*> pure o.phones
validatePerson2 (Person o) =
  mappedFirstName
         <*> (nonEmpty2 o.lastName  *> pure o.lastName)
         <*> pure o.address
         <*> pure o.phones
  where
    nonEmtpyFirstName :: Either String Unit
    nonEmtpyFirstName = nonEmpty2 o.firstName
    pureFirstName :: Either String String
    pureFirstName = pure o.firstName
    validatedFirstName :: Either String String
    validatedFirstName = nonEmtpyFirstName *> pureFirstName
    mappedFirstName :: Either String (String -> Address -> Array PhoneNumber -> Person)
    mappedFirstName = map person validatedFirstName


-- Write a Traversable instance for the following binary tree data structure, which combines side-effects from left-to-righ
data Tree a = Leaf | Branch (Tree a) a (Tree a)

--tree = Branch Leaf "123" Leaf

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
--  traverse :: forall a b m. (Applicative m) => (a -> m b) -> t a -> m (t b)
--  sequence :: forall a m. (Applicative m) => t (m a) -> m (t a)

--instance traversableList :: Traversable List where
--  traverse _ Nil = pure Nil
--  traverse f (Cons a as) = Cons <$> f a <*> traverse f as
--  sequence Nil = pure Nil
--  sequence (Cons a as) = Cons <$> a <*> sequence as


-- http://stackoverflow.com/questions/7460809/can-someone-explain-the-traverse-function-in-haskell
instance traversableTree :: Traversable Tree where
  traverse _ Leaf = pure Leaf
  traverse aToMB (Branch treeL a treeR) = Branch <$> traverse aToMB treeL <*> aToMB a <*> traverse aToMB treeR
  --traverse f = sequence <<< map f
  sequence Leaf = pure Leaf
  sequence (Branch treeL a treeR)  = Branch <$> sequence treeL <*> a <*> sequence treeR
  --sequence = traverse (\x -> x)



-- 7.10.1
-- Use a regular expression validator to ensure that the state field of the Address type contains two alphabetic characters.
-- Hint: see the source code for phoneNumberRegex.
stateRegex :: R.Regex
stateRegex =
  R.regex
    "^[A-Z]{2}$"
    (R.noFlags {ignoreCase = true})


validateAddress2 :: Address -> V Errors Address
validateAddress2 (Address o) =
  address <$> (nonEmpty3 "Street" o.street *> pure o.street)
          <*> (nonEmpty4 "City"   o.city   *> pure o.city)
          <*> (matches "State" stateRegex o.state *> pure o.state)



-- 7.10.2
-- Using the matches validator, write a validation function which checks that a string is not entirely whitespace.
-- Use it to replace nonEmpty where appropriate.

nonEmptyRegex :: R.Regex
nonEmptyRegex =
  R.regex "\\S" R.noFlags

nonEmpty3 :: String -> String -> V Errors Unit
nonEmpty3 fieldName s = matches fieldName nonEmptyRegex s

nonEmpty4 :: String -> String -> V Errors Unit
nonEmpty4 _ value
  | R.test nonEmptyRegex value = pure unit
nonEmpty4 field _ =
  invalid ["Field '" ++ field ++ " must contain at least one visible char."]
