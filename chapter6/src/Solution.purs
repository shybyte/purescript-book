module Solution where

import Prelude
import Data.Maybe
import Data.Array


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
  append (NonEmpty a1 array1) (NonEmpty a2 array2 )= NonEmpty a1 ( a2 : (array1 <> array2))


--instance showNonempty :: Show (NonEmpty a) where
--  show (NonEmpty a array) = "Nonempty (" ++ show (a:array)  ++ ")"