module Exercises.Chapter3 where
  
import Prelude
import Global
import Data.List (List, (:), filter, head, null, nubBy)
import Math (sqrt, pi)
import Control.Plus (empty)
import Data.Maybe (Maybe)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)
circleArea r = pi * r * r

-- Address Book
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

emptyBook :: AddressBook
emptyBook = empty

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> " : " <>
                  showAddress entry.address

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <>
                  addr.city <> ", " <>
                  addr.state

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry entry book = entry : book

-- filtering

findEntryByFilter :: (Entry -> Boolean) -> AddressBook -> Maybe Entry
findEntryByFilter filterFn = head <<< filter filterFn

matchByName :: String -> String -> Entry -> Boolean
matchByName firstName lastName entry = entry.firstName == firstName && entry.lastName == lastName

findEntryByName :: String -> String -> AddressBook -> Maybe Entry
findEntryByName firstName lastName book = findEntryByFilter (matchByName firstName lastName) book

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = findEntryByFilter (\entry -> entry.address.street == street)

hasEntryByName :: String -> String -> AddressBook -> Boolean
hasEntryByName firstName lastName = not <<< null <<< filter (matchByName firstName lastName)

removeDuplicatesByName :: AddressBook -> AddressBook
removeDuplicatesByName = nubBy equalByName
  where
    equalByName :: Entry -> Entry -> Boolean
    equalByName x y = matchByName y.firstName y.lastName x