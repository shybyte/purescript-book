module Common where

import Data.Char as C
import Data.Either (Either(Right, Left))
import Data.Foldable (foldr, foldl, class Foldable, foldMap)
import Data.Function (on)
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (class Monoid)
import Data.String (toCharArray)
import Data.Tuple (Tuple(Tuple))
import Prelude (class Functor, class Semigroup, (&&), class Eq, class Show, (<>), (<<<), eq, (*), (+), (==), show, (++), mod, (<$>), map)


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


-- 3. Exercise
-- Write a Functor instance for NonEmpty

instance functorNonEmpty :: Functor NonEmpty where
  map a2b (NonEmpty a as) = NonEmpty (a2b a) (map a2b as)


-- 4. Exercise
-- Write a Foldable instance for NonEmpty.
-- Hint: reuse the Foldable instance for arrays.

instance foldableNonEmpty :: Foldable NonEmpty where
  foldl bab b (NonEmpty a as) = foldl bab (bab b a)  as
  foldr abb b (NonEmpty a as) = abb a (foldr abb b as)
  foldMap am (NonEmpty a as) = am a ++ (foldMap am as)
