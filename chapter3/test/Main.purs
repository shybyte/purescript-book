module Test.Main where

import Control.Monad.Eff
import Control.Monad.Eff.Console (CONSOLE, print)
import Data.AddressBook (removeDuplicates, containsEntry, Address, AddressBook, Entry, findEntryByAddress, showEntry, emptyBook, insertEntry, findEntry)
import Data.Maybe (Maybe)
import Prelude (Unit, (<$>), ($), bind)

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

otherAddress :: Address
otherAddress =
  { street: "Ourstreet 12"
  , city: "Berlin"
  , state: "Germany"
  }

example2 :: Entry
example2 =
  { firstName: "Marco"
  , lastName: "Steel"
  , address: otherAddress
  }


main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
  let book1 = insertEntry example emptyBook

  print $ printEntry "John" "Smith" book0
  print $ printEntry "John" "Smith" book1

  -- Exercise 2
  print $ showEntry <$> findEntryByAddress example.address book1
  print $ showEntry <$> findEntryByAddress otherAddress book1

  -- Exercise 4
  print $ containsEntry "John" "Smith" book1
  print $ containsEntry "John" "Smith2" book1

  -- Exercise 4
  let book4 = insertEntry example2 $ insertEntry example book1
  print $ showEntry <$> book4
  print $ showEntry <$> removeDuplicates book4
