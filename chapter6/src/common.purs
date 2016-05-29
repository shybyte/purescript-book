module Common where

import Data.Char as C
import Data.Either (Either(Right, Left))
import Data.Foldable (foldMap)
import Data.Function (on)
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (class Monoid)
import Data.String (toCharArray)
import Data.Tuple (Tuple(Tuple))
import Prelude (class Semigroup, (&&), class Eq, class Show, (<>), (<<<), eq, (*), (+), (==), show, (++), mod)


-- 6.4 Common Type Classes

-- 1. Exercise
-- Define Show and Eq instances for Complex.

newtype Complex = Complex
  { real :: Number
  , imaginary :: Number
  }

derive instance genericComplex :: Generic Complex
instance showComplex :: Show Complex where
  show = gShow


instance eqComplex :: Eq Complex where
  eq (Complex c1) (Complex c2) =
    c1.real == c2.real && c1.imaginary == c2.imaginary


-- 2. Exercise
-- Write a Semigroup instance for non-empty arrays
-- by reusing the Semigroup instance for Array.

data NonEmpty a = NonEmpty a (Array a)

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty x xs) (NonEmpty y ys) =
    NonEmpty x (xs ++ [y] ++ ys)


instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty x xs) =
    "NonEmpty " ++ show x ++ " " ++ show xs
