import Data.List

sumList :: [Int] -> Int
sumList x
    |null x       = 0
    |otherwise    = (head(x) + sumList(tail x))

triangleNumbers :: Int -> Int
triangleNumbers x = sumList(take x [1..])

isFactor :: Int -> Int -> Bool
isFactor x y
  |(mod x y) == 0   = True
  |otherwise        = False
  
listProperFactors :: Int -> [Int]
listProperFactors x = filter (isFactor x) [2..(div x 2)]

isPrime :: Int -> Bool
isPrime y = if (listProperFactors y) == []
    then True
    else False

findPrimesUpTo :: Int -> [Int]
findPrimesUpTo x = filter isPrime [2..x]
  