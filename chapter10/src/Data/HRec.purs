module Data.HRec where

import Prelude
import Data.Function
import Data.Maybe
import Data.Tuple (lookup)

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


-- 10.14 Example: Homogeneous Records - 3.Exercise
-- Write a version of foldHRec which uses regular (curried) functions
foldHRec2 :: forall a r. (r -> String -> a -> r) -> r -> HRec a -> r
foldHRec2 f r rec  = runFn3 foldHRec (mkFn3 f) r rec


-- 10.14 Example: Homogeneous Records - 4.Exercise
-- Write a function lookup which looks up a key in a homogeneous record.

lookup :: forall a. String -> HRec a -> Maybe a
lookup needle rec = foldHRec2 foldF Nothing rec
  where
  foldF :: (Maybe a) -> String -> a -> (Maybe a)
  foldF Nothing key value = if (key == needle) then (Just value) else Nothing
  foldF foundJustValue@(Just _) _ _ = foundJustValue

-- Write two versions of this function.
-- The first version should use the foldHRec function.
-- The second version should be defined as a foreign function

foreign import lookupHelper :: forall a r. Fn4 r (a -> r) String (HRec a) r

lookup2 :: forall a. String -> HRec a -> Maybe a
lookup2 key rec= runFn4 lookupHelper Nothing (Just) key rec


-- 10.14 Example: Homogeneous Records - 5.Exercise
-- Write a version of the mapHRec function in which the mapping function receives
-- the property label as an additional argument.
foreign import mapHRec2 :: forall a b. Fn2 (Fn2 String a b) (HRec a) (HRec b)
