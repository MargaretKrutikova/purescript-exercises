module Test.Exercises.Chapter4.Factorization where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Exercises.Chapter4.Factorization 
import Test.Assert (ASSERT, assert)

runTests :: forall e. Eff (console :: CONSOLE, assert :: ASSERT | e) Unit
runTests = do
  log "Done"