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

