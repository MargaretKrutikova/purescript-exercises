module Test.Main where

import Prelude
import Data.Maybe (Maybe, fromMaybe, isJust)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Plus (empty)
import Data.List (List(Nil), (:), length)
import Test.Assert (ASSERT, assert)
import Exercises.Chapter3 (Address, Entry, findEntryByName, findEntryByStreet, hasEntryByName, insertEntry, matchByName, removeDuplicatesByName, showEntry)

address1 :: Address
address1 = { street: "123 Fake St.", city: "Faketown", state: "CA" }
address2 :: Address
address2 = { street: "124 Fake St.", city: "Faketown", state: "Sweden" }
address3 :: Address
address3 = { street: "Fiskargatan 4", city: "Gothenburg", state: "Sweden" }

entry1 :: Entry
entry1 = { firstName: "John", lastName: "Smith", address: address1 } 
entryDuplicateName :: Entry
entryDuplicateName = { firstName: "John", lastName: "Smith", address: address2 } 
entry2 :: Entry
entry2 = { firstName: "Anders", lastName: "Andersson", address: address3 } 

printEntries :: forall a. List Entry -> Eff (console :: CONSOLE | a) Unit
printEntries Nil = log "done"
printEntries (x:xs) = do
  log $ showEntry x
  printEntries xs

printEntry :: forall a. Maybe Entry -> Eff (console :: CONSOLE | a) Unit
printEntry entry = log $ fromMaybe "Nothing" (map showEntry entry)

main :: forall e. Eff (console :: CONSOLE, assert :: ASSERT | e) Unit
main = do
  -- address book tests
  log "insertEntry should insert entries correctly"

  let testBook = (insertEntry entry1 <<< insertEntry entryDuplicateName <<< insertEntry entry2) empty
  assert $ length testBook == 3
  printEntries testBook

  --------------------------------------------------------
  log "\nfindEntryByName should find entry correctly"
  let entryByName = findEntryByName "Anders" "Andersson" testBook
  assert $ isJust entryByName

  let nameIsMatching = map (matchByName "Anders" "Andersson") entryByName
  assert $ (fromMaybe false nameIsMatching) == true

  log ("Anders" <> " Andersson")
  printEntry entryByName
  
  --------------------------------------------------------
  log "\nfindEntryByStreet should find entry correctly"
  let entryByStreet = findEntryByStreet "124 Fake St." testBook
  assert $ isJust entryByStreet

  let isMatchStreet = map (\e -> e.address.street == "124 Fake St.") entryByStreet
  assert $ (fromMaybe false isMatchStreet) == true

  log "124 Fake St."
  printEntry entryByStreet

  --------------------------------------------------------
  log "\nhasEntryByName should return true if entry exists"
  assert $ hasEntryByName "Anders" "Andersson" testBook == true

  log "\nhasEntryByName should return false if entry doesn't exist"
  assert $ hasEntryByName "Some First Name" "Some Last Name" testBook == false

  log "\nremoveDuplicatesByName should remove entries with duplicate first and last names"
  let removedDuplicatesBook = removeDuplicatesByName testBook

  assert $ length removedDuplicatesBook == 2
  printEntries removedDuplicatesBook
  
     