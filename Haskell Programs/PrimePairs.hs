--Test for whether an integer is a multiple of another: isMultiple :: Factor Multiple -> True 
import Data.List


splitNumber:: Int -> [Int]
splitNumber x
  |x < 10    = x:[]
  |otherwise = (mod x 10):(splitNumber (div x 10))

timesTen :: Int -> Int -> Int
timesTen x y = (x * 10) + y

restoreValueFromList :: [Int] -> Int
restoreValueFromList x = foldl timesTen 0 x

isMultipleOf :: Int -> Int -> Bool
isMultipleOf x y
  |mod y x == 0 = True
  |otherwise    = False

--Test for whether an integer is a factor of another: isFactorOf :: Multiple Factor -> True 


isFactorOf :: Int->Int->Bool
isFactorOf x y
  |mod x y == 0 = True
  |otherwise    = False

listFactorsOf :: Int -> [Int]
listFactorsOf x = filter (isFactorOf x) [2..(div x 2)]

oneAbove :: Int -> Int

oneAbove x = x + 1



oneBelow :: Int -> Int

oneBelow x
  
  |x <= 0     = 0
  
  |otherwise  = x-1
  


isPossiblePrime :: Int -> Bool

isPossiblePrime x
  
  |x==2 || x==3           =True
  
  |isMultipleOf 6 (x+1)   =True   --x is one less than a multiple of 6
  
  |isMultipleOf 6 (x-1)   =True   --x is one more than a multiple of 6
  
  |otherwise              =False


isPossiblePrimeUpTo :: Int -> [Int]

isPossiblePrimeUpTo x = filter isPossiblePrime (take x [2..])



isPrime :: Int -> Bool
isPrime x = 
  if isPossiblePrime x
  
  then 
    if listFactorsOf x == []

      then True
      else False
 
   else False

listPrimesUpTo :: Int -> [Int]
listPrimesUpTo x = filter isPrime (isPossiblePrimeUpTo x)


