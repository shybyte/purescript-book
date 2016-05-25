module Test.Main where

import Prelude (Unit, (<$>), ($), bind)

import Control.Monad.Eff
import Control.Monad.Eff.Console (CONSOLE, print)
import Data.Maybe (Maybe)
import Data.AddressBook (Address, AddressBook, Entry, findEntryByAddress, showEntry, emptyBook, insertEntry, findEntry)

example :: Entry
example =
  { firstName: "John"
  , lastName: "Smith"
  , address: { street: "123 Fake St."
             , city: "Faketown"
             , state: "CA"
             }
  }

book0 :: AddressBook
book0 = emptyBook

printEntry :: String -> String -> AddressBook -> Maybe String
printEntry firstName lastName book = showEntry <$> findEntry firstName lastName book

unknownAddress :: Address
unknownAddress =
  { street: "Ourstreet 12"
  , city: "Berlin"
  , state: "Germany"
  }

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
  let book1 = insertEntry example emptyBook

  print $ printEntry "John" "Smith" book0
  print $ printEntry "John" "Smith" book1

  print $ showEntry <$> findEntryByAddress example.address book1
  print $ showEntry <$> findEntryByAddress unknownAddress book1
