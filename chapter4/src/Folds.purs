module Folds where

import Prelude
import Data.Foldable
import Data.Array ((:))

reverse :: forall a. Array a -> Array a
reverse = foldr (\x xs -> xs ++ [x]) []

reverser :: forall a. Array a -> Array a
reverser = foldl (\xs x -> x : xs) []