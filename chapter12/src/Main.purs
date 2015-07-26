module Main where
    
import Prelude
import Data.Foldable (foldMap)

import Control.Monad.Eff
import Control.Monad.Eff.Console (log)
import Control.Monad.Cont.Trans

import Network.HTTP.Client

main = runContT (getResponseText purescript_org) log
  where
  getResponseText req = responseToString <$> getAll req

  responseToString :: Response -> String
  responseToString (Response chunks) = foldMap runChunk chunks
  
  purescript_org :: Request
  purescript_org = Request 
    { host: "www.purescript.org"
    , path: "/" 
    }
