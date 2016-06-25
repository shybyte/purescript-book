module Data.HRec where

import Prelude

import Data.Function

foreign import data HRec :: * -> *

instance showHRec :: (Show a) => Show (HRec a) where
  show rec = runFn3 foldHRec f "empty" rec
    where
      f = mkFn3 \s k a -> "runFn3 insert " ++ show k ++ " " ++ show a ++ " $ " ++ s

foreign import empty :: forall a. HRec a

foreign import insert :: forall a. Fn3 String a (HRec a) (HRec a)

foreign import mapHRec :: forall a b. Fn2 (a -> b) (HRec a) (HRec b)

foreign import foldHRec :: forall a r. Fn3 (Fn3 r String a r) r (HRec a) r


instance functorHRec :: Functor HRec where
  map f rec = runFn2 mapHRec f rec


-- 10.14 Example: Homogeneous Records - 2.Exercise

-- Write a function union which calculates the union of two homogeneous records.
-- If the two records share a label, the second record should take precedence.

foreign import union :: forall a. Fn2 (HRec a) (HRec a) (HRec a)


-- 3.Exercise
-- Write a version of foldHRec which uses regular (curried) functions
foldHRec2 :: forall a r. (r -> String -> a -> r) -> r -> HRec a -> r
foldHRec2 f r rec  = runFn3 foldHRec (mkFn3 f) r rec
