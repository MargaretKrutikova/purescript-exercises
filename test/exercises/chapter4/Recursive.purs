module Test.Exercises.Chapter4 where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Assert (ASSERT, assert)
import Exercises.Chapter4.Recursive (
  countElementsByMatch, 
  countElementsByMatchOpt, 
  isEven,
  (<$?>))

runTests :: forall e. Eff (console :: CONSOLE, assert :: ASSERT | e) Unit
runTests = do
  let array = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let matchFn = \x -> mod x 4 == 0 || mod x 4 == 1

  log $ show array

  log $ show $ countElementsByMatch matchFn array
  log "------------------------------------------"
  log $ show $ countElementsByMatchOpt matchFn array

  assert $ isEven 9 == false
  assert $ isEven 120 == true
  assert $ isEven (-10) == false
  assert $ isEven 1 == false  
  assert $ isEven 2 == true 

  log "------------------------------------------"
  log $ show $ (\n -> n > 3 && n < 10) <$?> array

  log "Done"