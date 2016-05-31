module Unsafe where

import Prelude ((-))

-- 4. Exercise
-- Define a nullary type class and use it
-- to define a version of the unsafeIndex function from the Data.Array.Unsafe module,
-- which uses your constraint to express its lack of type-safety.
-- Use your function to define a function last
-- which chooses the last element of an array, and which preserves the Unsafe constraint.

class Unsafe

unsafeIndex :: forall a. (Unsafe) => Array a -> Int -> a
unsafeIndex = Data.Array.Unsafe.unsafeIndex

last :: forall a.  (Unsafe) => Array a -> a
last xs = unsafeIndex xs (Data.Array.length xs - 1)
