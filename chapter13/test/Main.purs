module Test.Main where

import Prelude

import Data.List (List(..), toList)
import Data.Array (intersect, sort, sortBy)
import Data.Function (on)
import Data.Foldable (foldr)

import Merge
import Tree 

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

isSorted :: forall a. (Ord a) => Array a -> Boolean
isSorted = go <<< toList
  where
  go (Cons x1 t@(Cons x2 _)) = x1 <= x2 && go t
  go _ = true

isSubarrayOf :: forall a. (Eq a) => Array a -> Array a -> Boolean
isSubarrayOf xs ys = xs `intersect` ys == xs

newtype Sorted a = Sorted (Array a)

instance showSorted :: (Show a) => Show (Sorted a) where
  show = show <<< sorted
  
sorted :: forall a. Sorted a -> Array a
sorted (Sorted xs) = xs

instance arbSorted :: (Arbitrary a, Ord a) => Arbitrary (Sorted a) where
  arbitrary = Sorted <<< sort <$> arbitrary

ints :: Array Int -> Array Int
ints = id

intToBool :: (Int -> Boolean) -> Int -> Boolean
intToBool = id

instance arbTree :: (Arbitrary a, Ord a) => Arbitrary (Tree a) where
  arbitrary = fromArray <<< sorted <$> arbitrary

instance coarbTree :: (Coarbitrary a) => Coarbitrary (Tree a) where
  coarbitrary Leaf = id
  coarbitrary (Branch l a r) = 
    coarbitrary l <<< 
    coarbitrary a <<< 
    coarbitrary r

treeOfNumber :: Tree Number -> Tree Number
treeOfNumber = id

main = do
  -- Tests for module 'Merge'

  showSample (arbitrary :: Gen (Sorted Int))

  quickCheck $ \xs ys -> isSorted $ merge (sorted xs) (sorted ys)
  quickCheck $ \xs ys -> xs `isSubarrayOf` merge xs ys

  quickCheck $ \xs ys -> isSorted $ ints $ mergePoly (sorted xs) (sorted ys)
  quickCheck $ \xs ys -> ints xs `isSubarrayOf` mergePoly xs ys
  
  quickCheck $ \xs ys f -> isSorted $ map f $ mergeWith (intToBool f) (sortBy (compare `on` f) xs) (sortBy (compare `on` f) ys)
  quickCheck $ \xs ys f -> xs `isSubarrayOf` mergeWith (intToBool f) xs ys
  
  -- Tests for module 'Tree'

  quickCheck $ \t a -> member a $ insert a (t :: Tree Number) 
  quickCheck $ \t xs -> isSorted $ toArray $ foldr insert t $ ints xs

  quickCheck $ \f g t -> 
    anywhere (\s -> f s || g s) t == 
      anywhere f (treeOfNumber t) || anywhere g t
