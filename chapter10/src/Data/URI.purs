module Data.URI where

import Prelude
import Math
import Data.Function

-- 10.10 Using JavaScript Code From PureScript
foreign import encodeURIComponent :: String -> String


-- 10.11 Wrapping JavaScript Values
foreign import unsafeHead :: forall a. Array a -> a



--10.12 Defining Foreign Types
foreign import data Undefined :: * -> *
foreign import head :: forall a. Array a -> Undefined a
foreign import isUndefined :: forall a. Undefined a -> Boolean

isEmpty :: forall a. Array a -> Boolean
isEmpty = isUndefined <<< head


-- 10.13 Functions of Multiple Arguments
divides :: Fn2 Number Number Boolean
divides = mkFn2 $ \n m -> m % n == 0.0
