import           Data.Char (isDigit)

{- Lesson 38: Errors in Haskell and the Either Type

After this lesson, I'll be able to:

    - Throw errors by using the error function
    - Understand the dangers of throwing errors
    - Use Maybe as a method for handling errors
    - Handle more sophisticated errors with the Either type

The traditional approach of throwing an exception is frowned upon in
Haskell since this makes it easy to have runtime errors that the compiler
cannot catch. Instead, we'll look at two approaches, using the Maybe type
and a much better solution, the Either type.

The problem with the Maybe type is it doesn't allow us to communicate much
about what went wrong. The Either type on the other hand allows us to
provide information about the error.
-}

{- 38.1: Head, partial functions, and errors

One of the first functions we looked at was the head function. However,
we get an exception if we use head on an empty list. Let's say we naively
implement a take function like below:
-}
myTake :: Int -> [a] -> [a]
myTake 0 _  = []
myTake n xs = head xs : myTake (n-1) (tail xs)

{-
when we ran this code, we see the problem:

    ghci> myTake 2 [1,2,3,4]
    [1,2]
    ghci> myTake 5 [1,2,3,4]
    [1,2,3,4,*** Exception: Prelude.head: empty list

Let's improve the function by using pattern matching:
-}
myTakePM :: Int -> [a] -> [a]
myTakePM 0 _      = []
myTakePM n (x:xs) = x : myTake (n-1) xs

{-
Now (with the -Wall compiler option on), we see:

    Pattern match(es) are non-exhaustive
     In an equation for 'myTakePM':
        Patterns not matched: p [] where p is not one of {0}

To cover all bases, we need to include a pattern for an empty list:
-}
myTakePM2 :: Int -> [a] -> [a]
myTakePM2 0 _      = []
myTakePM2 _ []     = error "empty list"  -- but here we are again throwing an exception
myTakePM2 n (x:xs) = x : myTake (n-1) xs

{-
Technically, head is a partial function. A function must take an argument
and return a result, but a partial function is not defined on all inputs.
Nearly all errors in software are the result of partial functions. In this
case, the program receives an input it cannot handle. Throwing an error
seems like the obvious solution.

Another common partial function is (/) that is undefined for 0. Ideally,
we want to transform partial functions into ones that work on all values.
For example, Haskell handles divide by zero by returning a special infinity
sentinel value. However, this isn't possible for many problems. Instead, we
must use types to capture when errors might happen.

Here are some other partial functions and the value on which they fail:

    - maximum - fails on empty list
    - succ - fails on maxBound for the type
    - sum - fails on infinite lists
-}

{- 38.2: Handling partial functions with Maybe

The Maybe type is a reasonable way to transform a partial function into
a complete function. E.g.
-}
maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x

{-
Since Maybe is an instance of Monad (and Functor and Applicative), we can
chain computations on values in a Maybe context. E.g. using Functor's <$>:

    ghci> (+2) <$> maybeHead [1]
    Just 3
    ghci> (+2) <$> maybeHead []
    Nothing

Or using Applicative's <*> (app) to chain functions in a context. E.g.
providing multiple arguments to the cons operator (:):

    ghci> pure (:) <*> maybeHead [1,2,3] <*> Just []
    Just [1]
    ghci> pure (:) <*> maybeHead [] <*> Just []
    Nothing

Let's write a safer version of myTake:
-}
myTakeSafer :: Int -> Maybe [a] -> Maybe [a]
myTakeSafer 0 _ = Just []
myTakeSafer n (Just xs) = (:) <$> maybeHead xs
                              <*> myTakeSafer (n-1) (Just (tail xs))

{-
This now works with error-causing inputs:

    ghci> myTakeSafer 3 (Just [1,2,3,4])
    Just [1,2,3]
    ghci> myTakeSafer 5 (Just [1,2,3,4])
    Nothing

However, this function is still not completely safe is that tail is also
a partial function.
-}

{- 38.3: Introducing the Either type

The limitation with Maybe is that Nothing becomes hard to interpret and
leads to ambiguities whether the result is valid or due to an error.
For example, in the previous unit, we implemented isPrime. The user
calls isPrime 997 and gets back Nothing. What does this mean? It would
be a lot better to show the user information about the error (that the
input exceeded the allowed value).

Here's the definition of Either:

    data Either a b = Left a | Right b

The convention is the Left value contains an error and Right for success.
Notice that Either has two type parameters, a and b, that allows you to
have a type for error and for success.

Let's use this in our head function:
-}
eitherHead :: [a] -> Either String a
eitherHead []     = Left "Not defined on empty list"
eitherHead (x:xs) = Right x

{-
    ghci> eitherHead [1,2,3]
    Right 1
    ghci> eitherHead []
    Left "Not defined on empty list"

Either is also an instance of Monad:

    ghci> (+1) <$> (eitherHead [1])
    Right 2
    ghci> (+1) <$> (eitherHead [])
    Left "Not defined on empty list"

Let's implement a minimal isPrime function with modelled error types:
-}
isPrimeLimit :: Int
isPrimeLimit = 10000

data PrimeError = TooLarge | InvalidValue

instance Show PrimeError
  where
    show TooLarge     = "Value exceeds max bound"
    show InvalidValue = "Value is not a valid candidate for prime checking"

isPrime :: Int -> Either PrimeError Bool
isPrime n
    | n < 2 = Left InvalidValue
    | n > isPrimeLimit = Left TooLarge
    | otherwise = Right True  -- Not implementing for brevity

{-
    ghci> isPrime 5
    Right True
    ghci> isPrime 1
    Left Value is not a valid candidate for prime checking
    ghci> isPrime 100000
    Left Value exceeds max bound
-}

displayResult :: Either PrimeError Bool -> String
displayResult (Right True)      = "It's prime"
displayResult (Right False)     = "It's composite"
displayResult (Left primeError) = show primeError

main :: IO ()
main = do
    print "Enter a number to test for primality:"
    n <- read <$> getLine
    let result = isPrime n
    print (displayResult result)

{-
    ghci> main
    "Enter a number to test for primality:"
    5
    "It's prime"

    ghci> main
    "Enter a number to test for primality:"
    1
    "Value is not a valid candidate for prime checking"

    ghci> main
    "Enter a number to test for primality:"
    100000
    "Value exceeds max bound"
-}

{- Summary

In this lesson, we've seen how to safely handle errors. We've seen how to
throw runtime errors with `error`, like how head does for empty lists.
The compiler cannot catch this problem. Ultimately, head is a partial
function that doesn't return a value for some inputs. We show how this
can be resolved with the Maybe type, but this can make the code harder
to understand. Finally, we saw that Either allows us to safely handle
errors as well as providing detailed information about them.
-}

{- Q38.1

Make a function addStrInts that takes two Ints represented as Strings and
adds them. The function would return an Either String Int. The Right
constructor should return the result, provided that the two arguments can
be parsed into Ints (use Data.Char isDigit to check). Return a different
Left result for the three possible cases:

    First value can’t be parsed.
    Second value can’t be parsed.
    Neither value can be parsed.
-}
addStrInts :: String -> String -> Either String Int
addStrInts lhs rhs =
    case (lhsIsDigits, rhsIsDigits) of
         (True, True)   -> Right ((read lhs) + (read rhs))
         (False, True)  -> Left "First value can't be parsed"
         (True, False)  -> Left "Second value can't be parsed"
         (False, False) -> Left "Neither value can be parsed"
  where
    lhsIsDigits = all isDigit lhs
    rhsIsDigits = all isDigit rhs

{-
    ghci> addStrInts "5" "6"
    Right 11
    ghci> addStrInts "x" "6"
    Left "First value can't be parsed"
    ghci> addStrInts "5" "y"
    Left "Second value can't be parsed"
    ghci> addStrInts "x" "y"
    Left "Neither value can be parsed"
-}

{- Q38.2

The following are all partial functions. Use the type specified to
implement a safer version of the function:

    succ—Maybe
    tail—[a] (Keep the type the same.)
    last—Either (last fails on empty lists and infinite lists; use an upper bound for the
   infinite case.)
-}

safeSucc :: (Enum a, Bounded a, Eq a) => a -> Maybe a
safeSucc n = if n == maxBound
             then Nothing
             else Just (succ n)

{-
    ghci> (safeSucc 1) :: Maybe Int
    Just 2
    ghci> (safeSucc (maxBound :: Int)) :: Maybe Int
    Nothing
-}

safeTail :: [a] -> [a]
safeTail []     = []
safeTail (x:xs) = xs

{-
    ghci> safeTail [1,2,3]
    [2,3]
    ghci> safeTail []
    []
-}

safeLast :: [a] -> Either String a
safeLast [] = Left "Not defined for empty list"
safeLast xs = _safeLast 1000 xs  -- Stop after 1000
  where
    _safeLast :: Int -> [a] -> Either String a
    _safeLast 0 _      = Left "List exceeds safe bound"
    _safeLast _ (x:[]) = Right x
    _safeLast n (x:xs) = _safeLast (n-1) xs

{-
    ghci> safeLast [1,2,3,4]
    Right 4
    ghci> safeLast [0 .. 999]
    Right 999
    ghci> safeLast [0 .. 1000]
    Left "List exceeds safe bound"
-}
