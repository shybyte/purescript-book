module Control.Monad.Eff.Alert where

import Prelude (Unit())

import Control.Monad.Eff

foreign import data ALERT :: !

foreign import alert :: forall eff. String -> Eff (alert :: ALERT | eff) Unit


-- 10.16 Defining New Effects - 1.Exercise
-- Write a wrapper for the confirm method on the JavaScript Window object.
foreign import data CONFIRM :: !
foreign import confirm :: forall eff. String -> Eff (alert :: CONFIRM | eff) Boolean
