module Action where

import Data.Monoid
import Prelude
-- ((++), (-))



--6.10

-- 1. Exercise

class (Monoid m) <= Action m a where
  act :: m -> a -> a

-- instance repeatAction :: Action Int String where
--  act 0 _ = ""
--  act n s = s ++ act (n - 1) s


-- 2. Exercise

instance arrayAction :: (Action m a, Monoid m) =>  Action m (Array a) where
 act m array = map (act m) array


-- 3. Exercise
-- Given the following newtype,
-- write an instance for Action m (Self m),
-- where the monoid m acts on itself using append:

newtype Self m = Self m

instance showSelf :: (Show a) => Show (Self a) where
  show (Self a) = "Self (" ++ show a ++ ")"

instance selfAction :: (Monoid m) =>  Action m (Self m) where
 act m (Self self) = Self (m <> self)
