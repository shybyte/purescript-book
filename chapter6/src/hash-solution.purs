module HashSolution where

import Data.Hashable
import Prelude
import Data.Array
import Data.Function (on)


-- 2.  Exercise

hasDuplicates :: forall a. (Hashable a) => Array a -> Boolean
hasDuplicates xs = length (nubBy (\x1 x2 -> hashEqual x1 x2 && x1 == x2) xs) /= length xs


-- 3 .Exercise

-- Write a Hashable instance for the following newtype
-- which upholds the type class law:

newtype Hour = Hour Int

instance eqHour :: Eq Hour where
 eq = eq `on` modHour12

modHour12 :: Hour -> Int
modHour12 (Hour hour) = hour `mod` 12

instance hashHour :: Hashable Hour where
  hash = modHour12 >>> hash
