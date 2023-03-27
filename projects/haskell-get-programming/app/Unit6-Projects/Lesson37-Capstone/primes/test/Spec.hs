import           Data.Maybe

import           Test.QuickCheck

import           Lib


prop_validPrimesOnly :: Int -> Bool
prop_validPrimesOnly val =
    if val < 2 || val >= length primes
    then result == Nothing
    else isJust result
  where
    result = isPrime val


prop_primesHaveNoDivisors :: Int -> Bool
prop_primesHaveNoDivisors val =
    if result == Just True
    then length divisors == 0
    else True
  where
    result = isPrime val
    divisors = filter (\x -> val `mod` x == 0) [2 .. (val - 1)]


prop_nonPrimesAreComposite :: Int -> Bool
prop_nonPrimesAreComposite val =
    if result == Just False
    then length divisors > 0
    else True
  where
    result = isPrime val
    divisors = filter (\x -> val `mod` x == 0) [2 .. (val - 1)]

prop_factorsMakeOriginal :: Int -> Bool
prop_factorsMakeOriginal val =
    if factors == Nothing
    then True
    else product (fromJust factors) == val
  where
    factors = primeFactors val


prop_allFactorsPrime :: Int -> Bool
prop_allFactorsPrime val =
    if factors == Nothing
    then True
    else all (== Just True) factorsPrime
  where
    factors = primeFactors val
    factorsPrime = map isPrime (fromJust factors)


main :: IO ()
main = do
    quickCheck prop_validPrimesOnly
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_primesHaveNoDivisors
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_nonPrimesAreComposite
    quickCheck prop_factorsMakeOriginal
    quickCheck prop_allFactorsPrime
