module Test.Main where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Console

import Data.Maybe
import Data.PhoneBook

example :: Entry
example = { firstName: "John", lastName: "Smith", phone: "555-555-5555" }
     
book0 :: PhoneBook
book0 = emptyBook

printEntry :: String -> String -> PhoneBook -> Maybe String
printEntry firstName lastName book = showEntry <$> findEntry firstName lastName book

main = do
  let john = { firstName: "John", lastName: "Smith", phone: "555-555-5555" }
      book1 = insertEntry john emptyBook

  print $ printEntry "John" "Smith" book0
  print $ printEntry "John" "Smith" book1
