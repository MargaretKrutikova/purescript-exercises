module Exercises.Chapter4.Recursive 
  (
    countElementsByMatchOpt,
    countElementsByMatch,
    isEven,
    calcSquares,
    findFactorPairs,
    (<$?>)
  ) where
  
import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array ((..), filter, head, tail, uncons, concatMap)

boolToNumber :: Boolean -> Int
boolToNumber false = 0
boolToNumber true = 1

countElementsRec :: forall a. (a -> Boolean) -> Array a -> Int -> Int
countElementsRec matchFn array currentCount = 
  case uncons array of 
    Just o -> countElementsRec matchFn o.tail $ updateCount o.head
    Nothing -> currentCount
    where
      updateCount :: a -> Int
      updateCount elem = currentCount + (boolToNumber $ matchFn elem)

countElementsByMatchOpt :: forall a. (a -> Boolean) -> Array a -> Int
countElementsByMatchOpt matchFn array = countElementsRec matchFn array 0

countElementsByMatch :: forall a. (a -> Boolean) -> Array a -> Int
countElementsByMatch _ [] = 0
countElementsByMatch matchFn array = 
    (addIfMatches $ head array) + countElementsByMatch matchFn (fromMaybe [] $ tail array)
    where
      addIfMatches :: Maybe a -> Int
      addIfMatches element = if fromMaybe false $ map matchFn element then 1 else 0

countEvenNumbers :: Array Int -> Int
countEvenNumbers array = countElementsByMatch (\x -> x `mod` 2 == 0) array

isEven :: Int -> Boolean
isEven 1 = false
isEven 2 = true
isEven number = 
  if number < 1
    then false
    else isEven $ number - 2

calcSquares :: Array Int -> Array Int
calcSquares = map (\n -> n * n)

infix 4 filter as <$?>

removeNegatives :: Array Int -> Array Int
removeNegatives arr = (\n -> n < 0) <$?> arr

findFactorPairs :: Int -> Array (Array Int)
findFactorPairs number = concatMap (\n -> generatePairs n) (1..number)
  where
    generatePairs :: Int -> Array (Array Int)
    generatePairs current = map (\n -> [current, n]) $ filter (\n -> n * current == number) (current..number)