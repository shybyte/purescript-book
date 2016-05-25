module Data.AddressBook where

import Prelude ((<<<), (==), (&&), (++))
import Data.List (List(Cons), filter, head)
import Data.Maybe (Maybe)

import Control.Plus (empty)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street ++ ", " ++ addr.city ++ ", " ++ addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName ++ ", " ++ entry.firstName ++ ": " ++ showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName


findEntryByAddress :: Address -> AddressBook -> Maybe Entry
findEntryByAddress address = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry =
    entry.address.street == address.street &&
    entry.address.city == address.city &&
    entry.address.state == address.state
