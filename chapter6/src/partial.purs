module Partial where

--6.9 Nullary Type Classes

class Partial

head :: forall a. (Partial) => Array a -> a
head = Data.Array.Unsafe.head

tail :: forall a. (Partial) => Array a -> Array a
tail = Data.Array.Unsafe.tail
