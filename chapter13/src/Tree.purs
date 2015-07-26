module Tree where

import Prelude

import Data.Array (length, drop, take)
import Data.Array.Unsafe (unsafeIndex)
import Data.Monoid

data Tree a 
  = Leaf
  | Branch (Tree a) a (Tree a)

insert :: forall a. (Ord a) => a -> Tree a -> Tree a
insert a Leaf = Branch Leaf a Leaf
insert a (Branch l a1 r) | a < a1 = Branch (insert a l) a1 r
insert a (Branch l a1 r) = Branch l a1 (insert a r)

member :: forall a. (Ord a) => a -> Tree a -> Boolean
member _ Leaf = false
member a (Branch _ a1 _) | a == a1 = true
member a (Branch l a1 _) | a < a1 = a `member` l
member a (Branch _ _  r) = a `member` r

toArray :: forall a. Tree a -> Array a
toArray Leaf = []
toArray (Branch l a r) = toArray l ++ [a] ++ toArray r

fromArray :: forall a. (Ord a) => Array a -> Tree a
fromArray [] = Leaf
fromArray xs = Branch l a r
  where
  mid = length xs / 2
  a  = xs `unsafeIndex` mid
  l = fromArray $ take mid xs
  r = fromArray $ drop (mid + 1) xs

anywhere :: forall a. (Tree a -> Boolean) -> Tree a -> Boolean
anywhere f Leaf = f Leaf
anywhere f t@(Branch l _ r) = anywhere f l || f t || anywhere f r
