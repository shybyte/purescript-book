module Network.HTTP.Client where

import Prelude

import Data.List
import Data.Maybe
import Data.Function

import Control.Monad.Eff
import Control.Monad.Eff.Ref

import Control.Monad.Trans
import Control.Monad.Cont.Trans
import Control.Monad.Cont.Extras

foreign import data HTTP :: !

newtype Request = Request
  { host :: String
  , path :: String
  }

newtype Chunk = Chunk String

instance showChunk :: Show Chunk where
  show (Chunk s) = "Chunk " ++ show s

newtype Response = Response (List Chunk)

instance showResponse :: Show Response where
  show (Response cs) = "Response " ++ show cs

runChunk :: Chunk -> String
runChunk (Chunk s) = s

type WithHTTP eff = (http :: HTTP | eff)

foreign import getImpl :: 
                 forall eff. Fn3 Request 
                   (Chunk -> Eff (WithHTTP eff) Unit) 
                   (Eff (WithHTTP eff) Unit) 
                   (Eff (WithHTTP eff) Unit)

getChunk :: forall eff. Request -> 
                        (Maybe Chunk -> Eff (WithHTTP eff) Unit) -> 
                        Eff (WithHTTP eff) Unit
getChunk req k = runFn3 getImpl req (k <<< Just) (k Nothing)

getCont :: forall eff. Request -> ContT Unit (Eff (WithHTTP eff)) (Maybe Chunk)
getCont req = ContT $ getChunk req
 
getAll :: forall eff. Request -> ContT Unit (Eff (WithHTTP (WithRef eff))) Response
getAll req = Response <$> collect (getCont req)

