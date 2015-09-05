module Solution where

import Prelude
import Data.Maybe
import Data.Array
import Data.Foldable
import Data.Monoid


newtype Complex = Complex
 { real :: Number
 , imaginary :: Number
 }

instance showComlex :: Show Complex where
  show (Complex c) = "Complex (" ++ show c.real ++ "+i" ++ show c.imaginary ++ ")"

instance eqComlex :: Eq Complex where
  eq (Complex c1) (Complex c2) = c1.real == c2.real && c1.imaginary == c2.imaginary



data NonEmpty a = NonEmpty a (Array a)

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty a1 array1) (NonEmpty a2 array2 )= NonEmpty a1 ( array1 <> (a2 : array2))


instance semigroupFunctor :: Functor NonEmpty where
  map mapFunction (NonEmpty a array )= NonEmpty (mapFunction a) (mapFunction <$> array)


instance showNonempty :: (Show a) => Show (NonEmpty a) where
  show (NonEmpty a array) = "Nonempty (" ++ show (a:array)  ++ ")"


instance foldableNonEmpty :: Foldable NonEmpty where
  --foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
  foldr abb b  (NonEmpty a array) = abb a (foldr abb b array)
  --foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  foldl bab b  (NonEmpty a array) = foldl bab (bab b a) array
  foldMap f xs = foldr (\x acc -> f x <> acc) mempty xs

instance eqNonempty :: (Eq a) => Eq (NonEmpty a) where
  eq (NonEmpty a1 array1) (NonEmpty a2 array2)  =
    a1 == a2 && array1 == array2



data Extended a = Finite a | Infinite

instance eqExtended :: (Eq a) => Eq (Extended a) where
  --compare :: a -> a -> Ordering
  eq Infinite Infinite  = true
  eq Infinite _  = false
  eq _ Infinite  = false
  eq (Finite a1) (Finite a2)  = a1 == a2

instance ordExtended :: (Ord a) => Ord (Extended a) where
  --compare :: a -> a -> Ordering
  compare Infinite Infinite  = EQ
  compare Infinite _  = GT
  compare _ Infinite  = LT
  compare (Finite a1) (Finite a2)  = compare a1  a2



data OneMore f a = OneMore a (f a)

instance foldableOneMore :: (Foldable f) => Foldable (OneMore f) where
  --foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
  foldr abb b  (OneMore a array) = abb a (foldr abb b array)
  --foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  foldl bab b  (OneMore a array) = foldl bab (bab b a) array
  foldMap f xs = foldr (\x acc -> f x <> acc) mempty xs



class (Monoid m) <= Action m a where
 act :: m -> a -> a

--instance repeatAction :: Action Int String where
--  act 0 _ = ""
-- act n s = s ++ act (n - 1) s

instance arrayAction :: (Action m a, Monoid m) =>  Action m (Array a) where
 act m array = map (act m) array


newtype Self m = Self m

instance showSelf :: (Show a) => Show (Self a) where
  show (Self a) = "Self (" ++ show a ++ ")"

instance selfAction :: (Monoid m) =>  Action m (Self m) where
 act m (Self self) = Self (m <> self)
