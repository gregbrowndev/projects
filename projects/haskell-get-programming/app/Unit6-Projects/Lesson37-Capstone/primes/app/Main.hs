module Main (main) where
import           Data.Maybe
import           Lib

main :: IO ()
main = do
    putStrLn "Enter a number to check if it's prime:"
    number <- (read <$> getLine)
--    number <- readLn :: IO Int
    let numberIsPrime = isPrime number
    putStrLn (if numberIsPrime == (Just True)
              then "It is prime!"
              else "It is not prime!")


main2 :: IO ()
main2 = do
    putStrLn "Enter a number to factor:"
    number <- (read <$> getLine)
    let factors = primeFactors number
    let response = if factors == Nothing
                   then "Sorry, this number is not a valid candidate for primality testing"
                   else "It has factors: " ++ (show $ fromJust factors)
    putStrLn response
