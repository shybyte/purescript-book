module Solution where

import Prelude
import Data.Maybe


newtype Complex = Complex
 { real :: Number
 , imaginary :: Number
 }

instance showComlex :: Show Complex where
  show (Complex c) = "Complex (" ++ show c.real ++ "+i" ++ show c.imaginary ++ ")"

instance eqComlex :: Eq Complex where
  eq (Complex c1) (Complex c2) = c1.real == c2.real && c1.imaginary == c2.imaginary

