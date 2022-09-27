import Data.List



--Test for whether an integer is a factor of another: isFactorOf :: Multiple Factor -> True 

isFactorOf :: Int->Int->Bool
isFactorOf x y
  |mod x y == 0 = True
  |otherwise    = False

--Find a square root in a complex chain of types because sqrt is type Floating so needs Floating x as a parameter
--We are using Int rather than Integral because this test is only used on small values

toIntSqrt :: Float -> Int
toIntSqrt x = round (sqrt(x))

--This was the old way and is useful in programs to find perfect numbers. Replace below by hasNoFactor
listFactorsOf :: Int -> [Int]
--listFactorsOf x = filter (isFactorOf x) [2..(div x 2)]    removed because not as efficient as going up to the square root
listFactorsOf x = filter (isFactorOf x) [2..(toIntSqrt(fromIntegral(x)))]

--Much more efficient for prime finding because we don't care after the first factor has been found
hasNoFactor :: Int -> Bool
hasNoFactor x
  |length (take 1 (filter (isFactorOf x) [2..(toIntSqrt(fromIntegral(x)))])) == 0 = True
  |otherwise = False

isMultipleOf :: Int -> Int -> Bool
isMultipleOf x y
  |mod y x == 0 = True
  |otherwise    = False

isPossiblePrime :: Int -> Bool

isPossiblePrime x

  |x==2 || x==3           =True

  |isMultipleOf 6 (x+1)   =True   --x is one less than a multiple of 6

  |isMultipleOf 6 (x-1)   =True   --x is one more than a multiple of 6

  |otherwise              =False


isPossiblePrimeUpTo :: Int -> [Int]

isPossiblePrimeUpTo x = filter isPossiblePrime (take x [2..])



isPrime :: Int -> Bool
isPrime x = if isPossiblePrime x

then if (hasNoFactor x)

    then True
    else False

else False

listPrimesUpTo :: Int -> [Int]
listPrimesUpTo x = filter isPrime (isPossiblePrimeUpTo x)


--Not currently using this function because we only need the power to test by Lucas Lehmer
mersenneResult :: Int -> Integer
mersenneResult x =(2^x)-1
--mersenneResult x = (round((2**fromIntegral(x))-1))  -- If we need to work with much bigger powers

--Find, recursively, the Lucas Lehmer series starting from 4 and return the mth term.
lucasLehmer :: Int -> Int -> Integer
lucasLehmer p m
  |m == 0    = 4
  |otherwise = mod ((lucasLehmer p (m-1))^2-2) ((2^p)-1)

--For a Mersenne Prime Mp: find the Lucas Lehmer residue. This is the (p-2)th term of the Lucas Lehmer series
lLPrimeTest :: Int -> Bool
lLPrimeTest p
  |((lucasLehmer p (p-2)) `mod` ((2^p)-1)) == 0 = True
  |otherwise                                    = False

isMersenneNumber :: Int -> Bool
isMersenneNumber x =  if isPrime x

                      then if lLPrimeTest x
  -- evaluating in two stages to ensure it does not test the big number if the small one fails

                          then  True

                          else  False
                      else  False

listMersennePrimesUpToPower :: Int -> [Integer]
listMersennePrimesUpToPower x = map mersenneResult (filter isMersenneNumber (isPossiblePrimeUpTo x))



countMersennePrimesUpToPower :: Int -> Int
countMersennePrimesUpToPower x = length listMersennePrimesUpToPower x

--main = do
--  putStrLn "Mersenne Primes up to Power 7"
--  print(listMersennePrimesUpToPower 7)