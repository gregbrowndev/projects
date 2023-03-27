module Lib
    ( primes,
      isPrime,
      primeFactors
    ) where

--primes :: Int -> [Int]
--primes n = sieve [2 .. n]

primes :: [Int]
primes = sieve [2 .. 1000]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime:rest) = nextPrime : sieve noFactors
  where
    noFactors = filter (\x -> (x `mod` nextPrime) /= 0) rest

isPrime :: Int -> Maybe Bool
isPrime n | n < 2 = Nothing
          | n >= length primes = Nothing
          | otherwise = Just (n `elem` primes)


unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors 0 [] = []
unsafePrimeFactors _ [] = []
unsafePrimeFactors n (next:rest) =
    if n `mod` next == 0
    then next:unsafePrimeFactors (n `div` next) (next:rest)
    else unsafePrimeFactors n rest


primeFactors :: Int -> Maybe [Int]
primeFactors n | n < 2 = Nothing
               | n >= length primes = Nothing
               | otherwise = Just (unsafePrimeFactors n primesLessThanN)
  where
    primesLessThanN = filter (<= n) primes
