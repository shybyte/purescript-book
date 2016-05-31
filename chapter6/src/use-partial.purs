module UsePartial where

import Partial (head, tail, class Partial)

result :: Partial => Int
result = Partial.head [1, 2, 3]

secondElement :: forall a. (Partial) => Array a -> a
secondElement xs = head (tail xs)

-- Error!
-- secondElement' :: forall a. Array a -> a
-- secondElement' xs = head (tail xs)
