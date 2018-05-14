module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Array ((..), uncons)
import Data.Maybe (Maybe(..))
import Exercises.Chapter3

-- get numbers divisible by 3 or 5

getNaturalNumbersBelow :: Int -> Array Int  
getNaturalNumbersBelow n = 1 .. (n - 1)

shouldSum :: Int -> Boolean
shouldSum n = n `mod` 3 == 0 || n `mod` 5 == 0

sum :: Array Int -> Int
sum arr = case uncons arr of
  Just { head: x, tail: xs } -> 
    (if shouldSum x then x else 0) + sum xs
  Nothing -> 0

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let a = sum (getNaturalNumbersBelow 1000) 
  logShow (diagonal 3.0 4.0)
  log (show a)