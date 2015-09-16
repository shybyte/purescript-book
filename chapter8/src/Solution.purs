module Solution where

import Prelude
import Data.Maybe
import Data.Array

-- 8.7.1
-- Look up the types of the head and tail functions from the Data.Array module in the purescript-arrays package.
-- Use do notation with the Maybe monad to combine these functions
-- into a function third which returns the third element of an array with three or more elements.
-- Your function should return an appropriate Maybe type.

third :: forall a. Array a -> Maybe a
third as = do
  tail1 <- tail as
  tail2 <- tail tail1
  head tail2