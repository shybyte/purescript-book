module Common where

import Data.Foldable (foldr, foldl, class Foldable, foldMap)
import Prelude (compare, Ordering(GT, LT, EQ), class Ord, class Functor, class Semigroup, (&&), class Eq, class Show, (<>), (<<<), eq, (*), (+), (==), show, (++), mod, (<$>), map)
import Data.Generic (class Generic, gShow)


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


-- 6.7

-- 1. Exercise
-- Write an Eq instance for the type NonEmpty a
-- which reuses the instances for Eq a and Eq (Array a)

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty a1 as1) (NonEmpty a2 as2) =
    a1 == a2 && as1 == as2


-- 2. Exercise
-- Write an Ord instance for Extended a which reuses the Ord instance for a.

data Extended a = Finite a | Infinite

instance ordExtended :: Ord a => Eq (Extended a) where
  eq a1 a2 = compareExtended a1 a2 == EQ

instance eqExtended :: Ord a => Ord (Extended a) where
  compare = compareExtended

compareExtended :: forall a. (Ord a) => Extended a -> Extended a -> Ordering
compareExtended Infinite Infinite = EQ
compareExtended Infinite (Finite _) = LT
compareExtended (Finite _) Infinite = GT
compareExtended (Finite a1) (Finite a2) = compare a1 a2


-- 3. Exercise

data OneMore f a = OneMore a (f a)

instance eqOneMoreInstance :: (Eq a, Eq (f a)) => Eq (OneMore f a) where
  eq = eqOneMore

eqOneMore :: forall a f. (Eq a, Eq (f a)) => OneMore f a -> OneMore f a -> Boolean
eqOneMore (OneMore a1 as1) (OneMore a2 as2) =
  a1 == a2 && as1 == as2

instance foldableOneMore :: (Foldable f) => Foldable (OneMore f) where
  foldl bab b (OneMore a as) = foldl bab (bab b a)  as
  foldr abb b (OneMore a as) = abb a (foldr abb b as)
  foldMap am (OneMore a as) = am a ++ (foldMap am as)
