module Exercises.Chapter4.Factorization (
  Factor,
  findFactorizationsOptimized,
  findFactorizations
) where
  
import Prelude
import Data.Maybe (Maybe(Just, Nothing))
import Data.Array (concatMap, filter, head, reverse, (..), (:))
import Data.Tuple (Tuple(..))

-- straight forward non-optimized implementation of finding factorizatons
findFactorizations :: Int -> Array (Array Int)
findFactorizations number = factorize number 2

factorize :: Int -> Int -> Array (Array Int)
factorize number prevFactor = 
  if number <= 1 then [[]]
  else concatMap (\n -> map (\x -> n : x) (factorize (number / n) n) ) findBiggerFactors
  where 
    findBiggerFactors :: Array Int
    findBiggerFactors = filter (\n -> (mod number n == 0) && (number / n >= n || number == n)) (prevFactor..number)

-- attempt to optimize factorization problem with tail recursion

data Factor = Factor { factors :: Array Int, remainder :: Int }

instance showFactor :: Show Factor where 
  show (Factor { factors: f, remainder: r }) = show f

separateByMatch :: forall a. (a -> Boolean) -> Array a -> Tuple (Array a) (Array a)
separateByMatch matchFn array = Tuple (filter matchFn array) (filter (not matchFn) array) 

-- main function to call
findFactorizationsOptimized :: Int -> Array Factor
findFactorizationsOptimized number = reverse $ factorizeOptimized number [Factor {factors: [], remainder: number }]

-- tail call optimized recursive function
factorizeOptimized :: Int -> Array Factor -> Array Factor 
factorizeOptimized number factors = 
   case (separateByMatch isNonCompleteFactorization factors) of
    Tuple [] _ -> factors
    Tuple match rest -> factorizeOptimized number $ rest <> (completeFactorizations match)
    where 
      completeFactorizations :: Array Factor -> Array Factor
      completeFactorizations = concatMap (\n -> completeFactorization number n)

-- advance one step further in factorization by finding all factors of the remainder 
-- and combining the original result with the new factors 
completeFactorization :: Int -> Factor -> Array Factor
completeFactorization originalNumber (Factor { factors: factors, remainder: remainder }) = 
  map (\n -> Factor {factors: (n : factors), remainder: remainder / n}) $ 
    findSmallerFactors remainder $ getGreatestFactor factors originalNumber

isNonCompleteFactorization :: Factor -> Boolean
isNonCompleteFactorization (Factor {remainder: r}) = r > 1

findSmallerFactors :: Int -> Int -> Array Int 
findSmallerFactors number ceiling = filter (\n -> mod number n == 0) (2..ceiling)

getGreatestFactor :: Array Int -> Int -> Int
getGreatestFactor factors number = 
  case head factors of
    Nothing -> number
    Just factor -> factor
