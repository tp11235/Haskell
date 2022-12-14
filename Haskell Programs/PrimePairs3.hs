rtfd             TXT.rtf   .z
  +      r
  {\rtf1\ansi\ansicpg1252
{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\pard\tx560\tx1120\tx1680\tx2240\tx2800\tx3360\tx3920\tx4480\tx5040\tx5600\tx6160\tx6720\pardirnatural\partightenfactor0

\f0\fs24 \cf0 import Data.List\
\
--These functions work towards a test for whether the reverse of a prime is prime\
\
splitNumber:: Int -> [Int]\
splitNumber x\
  |x < 10    = x:[]\
  |otherwise = (mod x 10):(splitNumber (div x 10))\
\
timesTen :: Int -> Int -> Int\
timesTen x y = (x * 10) + y\
\
restoreValueFromList :: [Int] -> Int\
restoreValueFromList x = foldl timesTen 0 x\
\
revValue:: Int -> Int\
revValue x = restoreValueFromList (splitNumber x)\
\
isInt :: Float -> Bool\
isInt x = x == fromInteger (round x)\
\
isSquareNumber :: Float -> Bool\
isSquareNumber x = isInt(sqrt(x)) && (x /= 0)\
\
isPrimePair :: Int -> Bool\
isPrimePair x = isPrime (revValue x)\
\
isSquarePrimePair :: Int -> Bool\
isSquarePrimePair x =\
  if isPrimePair x then\
     if isSquareNumber (fromIntegral(((revValue x) - x))) then True\
     else False\
  else False\
  \
compileTuple :: Int->(Int,Int,Int)\
compileTuple x = (x, revValue x, ((revValue x) - x))\
\
getListOfPrimePairsUpTo::Int->[(Int,Int,Int)]\
getListOfPrimePairsUpTo x = map compileTuple (filter isSquarePrimePair (listPrimesUpTo x))\
\
--These functions work towards a list of prime numbers up to a given value\
\
isMultipleOf :: Int -> Int -> Bool\
isMultipleOf x y\
  |mod y x == 0 = True\
  |otherwise    = False\
\
isFactorOf :: Int->Int->Bool\
isFactorOf x y\
  |mod x y == 0 = True\
  |otherwise    = False\
\
listFactorsOf :: Int -> [Int]\
listFactorsOf x = filter (isFactorOf x) [2..(div x 2)]\
--listFactorsOf x = filter (isFactorOf x) [2..(round (sqrt x))]\
\
hasNoFactor :: Int -> Bool\
hasNoFactor x\
   |length (take 1 (filter (isFactorOf x) [2..(div x 2)])) == 0 = True\
   |otherwise = False\
\
\
isPossiblePrime :: Int -> Bool\
isPossiblePrime x\
  |x==2 || x==3           =True\
  |isMultipleOf 6 (x+1)   =True   --x is one less than a multiple of 6\
  |isMultipleOf 6 (x-1)   =True   --x is one more than a multiple of 6\
  |otherwise              =False\
\
isPossiblePrimeUpTo :: Int -> [Int]\
isPossiblePrimeUpTo x = filter isPossiblePrime (take x [2..])\
\
isPrime :: Int -> Bool\
isPrime x = \
  if isPossiblePrime x\
  then \
-- if listFactorsOf x == []\
   if (hasNoFactor x)\
      then True\
      else False\
   else False\
\
listPrimesUpTo :: Int -> [Int]\
listPrimesUpTo x = filter isPrime (isPossiblePrimeUpTo x)\
\
--The entry point\
\
main = do\
  putStrLn "Prime Pairs up to 1000000"\
  print(getListOfPrimePairsUpTo 1000000)\
  \
  }   #         TXT.rtf   YVv\?          