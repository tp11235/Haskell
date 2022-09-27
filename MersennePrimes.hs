import Data.List

--These functions work towards a list of prime numbers up to a given value

toIntSqrt :: Float -> Int
toIntSqrt x = round (sqrt x)

isMultipleOf :: Int -> Int -> Bool
isMultipleOf x y
  |mod y x == 0 = True
  |otherwise    = False

isFactorOf :: Int->Int->Bool
isFactorOf x y
  |mod x y == 0 = True
  |otherwise    = False

hasNoFactor :: Int -> Bool
hasNoFactor x
  |length (take 1 (filter (isFactorOf x) [2..(toIntSqrt(fromIntegral(x)))])) == 0 = True
  |otherwise = False

--All primes above 3 are one either side of a multiple of 6
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
    if (hasNoFactor x)
      then True
      else False
   else False

lucasLehmerFunction:: Int -> Int -> Int
lucasLehmerFunction s m = ((s * s) - 2) `mod` m

lucasLehmerSequence:: Int -> Int -> Int -> [Int]
lucasLehmerSequence 4 m p = map (lucasLehmerFunction lucasLehmerFunction m) take (p - 2) [1..])


listPrimesUpTo :: Int -> [Int]
listPrimesUpTo x = filter isPrime (isPossiblePrimeUpTo x)

myIntegerPower:: Int ->Int -> Int
myIntegerPower x y = x^y

mersenneResult :: Int -> Int

mersenneResult x =(2^x)-1

--mersenneResult x = (round((2**fromIntegral(x))-1))

isMersenneNumber :: Int -> Bool
isMersenneNumber x =  if isPrime x

                        then if isPrime (mersenneResult x)
  -- evaluating in two stages to ensure it does not test the big number if the small one fails

                          then  True

                          else  False

                      else  False

listMersennePrimesUpToPower :: Int -> [Int]
listMersennePrimesUpToPower x = map mersenneResult (filter isMersenneNumber (isPossiblePrimeUpTo x))

