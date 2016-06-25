module Control.Monad.Eff.Storage where

import Prelude (Unit())

import Data.Foreign

import Control.Monad.Eff

foreign import data STORAGE :: !

foreign import setItem :: forall eff. String -> String -> Eff (storage :: STORAGE | eff) Unit

foreign import getItem :: forall eff. String -> Eff (storage :: STORAGE | eff) Foreign


-- 10.16 Defining New Effects - 1.Exercise
-- Write a wrapper for the removeItem method on the localStorage object
foreign import removeItem :: forall eff. String -> Eff (storage :: STORAGE | eff) Unit
