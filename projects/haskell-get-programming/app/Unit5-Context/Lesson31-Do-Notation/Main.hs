import qualified Data.Map as Map
{- Lesson 31: Making Monads Easier with Do-Notation

After this lesson, I'll be able to:

    - Use do-notation to simplify working with Monads
    - Translate from Monad methods and lambdas to do-notation
    - Generate code from one instance of Monad to all Monads

The Monad type class allows for powerful abstraction when using types in a context.
But its methods, >>, >>=, and return quickly become cumbersome. We'll look at two
ways to make working with Monads easier.

The first is the do-notation that we've already seen. In the next lesson, we'll
learn how List works as a Monad. This leads to the next abstraction over Monads
that makes them even easier to work with: list comprehensions. While it's
important to understand the methods of the Monad type class, in practice you
will use both of these abstractions for the majority of your work.
-}

{- Consider this: Write a program by using the tools of the Monad type class that
takes a pair of values in a context, and then returns the maximum of each pair.
Here’s your type signature to get you started:
-}
maxPairM :: (Monad m, Ord a) => m (a,a) -> m a
maxPairM mPair = mPair >>= (\(val1, val2) -> return (max val1 val2))

{-
    ghci> maxPairM (Just (10, 6))
    Just 10
    ghci> maxPairM (pure (10, 6) :: IO (Int, Int))
    10
    ghci> maxPairM [(10, 6)]
    [10]
-}

{-
In the last lesson, we wrote a program helloName:
-}
askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloName :: IO ()
helloName = askForName
                >> getLine
                >>= (\name -> return (nameStatement name))
                >>= putStrLn

{-
Let's rewrite this using do-notation:
-}
helloNameDo :: IO ()
helloNameDo = do
    askForName
    name <- getLine
    putStrLn (nameStatement name)

{-
Here's an overview of the changes:

    - Actions performed using >> are written as single line statements
    - The <- abstracts out creating a lambda func and connecting with
      >>=. Notice that the assigned variable is the name of the argument
      in the lambda function.

As an exercise to translate back from do-notation, desugar the code below
from unit 4:
-}
helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

helloSugared :: IO ()
helloSugared = do
 name <- getLine
 let statement = helloPerson name
 putStrLn statement

helloDesugared :: IO ()
helloDesugared = getLine
                    >>= (\name -> return (helloPerson name))
                    >>= putStrLn

{-
Using do-notation is strongly preferred for non-trivial use of monadic operators.
But for simple functions such as an echo function, using >>= is often easier, e.g.
-}
echo :: IO ()
echo = getLine >>= putStrLn

{- 31.2 Using do-notation to reuse the same code in different contexts

Next we'll look at a series of examples to further demonstrate how Monad
allows us to reuse the same code across different contexts. We want to process
data for job candidates to our company to determine whether they've passed or
failed. We'll see how the same code can handle candidates in the context of
IO, Maybe, and even List.
-}

data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)

data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate {
    candidateId :: Int,
    codeReview  :: Grade,
    cultureFit  :: Grade,
    education   :: Degree
} deriving Show

viable :: Candidate -> Bool
viable candidate = all (== True) tests
  where
    passedCoding = codeReview candidate > B
    passedCultureFit = cultureFit candidate > C
    educationMin = education candidate >= MS
    tests = [passedCoding, passedCultureFit, educationMin]

readInt :: IO Int
readInt = getLine >>= (return . read)

readGrade :: IO Grade
readGrade = getLine >>= (return . read)

readDegree :: IO Degree
readDegree = getLine >>= (return . read)

readCandidate :: IO Candidate
readCandidate = do
    putStrLn "Enter candidate ID:"
    candidateId <- readInt
    putStrLn "Enter code review grade:"
    codeReviewGrade <- readGrade
    putStrLn "Enter culture fit grade:"
    cultureFitGrade <- readGrade
    putStrLn "Enter education:"
    education <- readDegree
    return (Candidate {
            candidateId = candidateId,
            codeReview = codeReviewGrade,
            cultureFit = cultureFitGrade,
            education = education
        })

assessCandidateIO :: IO String
assessCandidateIO = do
    candidate <- readCandidate
    let passed = viable candidate
    let statement = if passed then "passed" else "failed"
    return statement

{-
    ghci> assessCandidateIO
    Enter candidate ID:
    1
    Enter code review grade:
    A
    Enter culture fit grade:
    B
    Enter education:
    PhD
    "passed"

    ghci> assessCandidateIO
    Enter candidate ID:
    2
    Enter code review grade:
    F
    Enter culture fit grade:
    C
    Enter education:
    MS
    "failed"
-}

{- 31.2.3 The Maybe context - working with a map of candidates

In the next example, we'll work with a database of candidates which we want to
use to look up a given candidate and find whether they are viable. This requires
us to handle missing data - the Maybe context!
-}

candidate1 :: Candidate
candidate1 = Candidate {
    candidateId = 1,
    codeReview = A,
    cultureFit = A,
    education = BA
}

candidate2 :: Candidate
candidate2 = Candidate {
    candidateId = 2,
    codeReview = C,
    cultureFit = A,
    education = PhD
}

candidate3 :: Candidate
candidate3 = Candidate {
    candidateId = 3,
    codeReview = A,
    cultureFit = B,
    education = MS
}

candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList [(1, candidate1),
                            (2, candidate2),
                            (3, candidate3)]

{-
Now we have our map of candidates, we have the same problem in a different
context. Before we were worried about interacting with a user, now we're
concerned with passing around potentially missing values. We need a function
that looks a lot like accessCandidateIO but works for Maybe types.
-}
accessCandidateMaybe :: Int -> Maybe String
accessCandidateMaybe cId = do
    candidate <- Map.lookup cId candidateDB
    let passed = viable candidate
    let statement = if passed then "passed" else "failed"
    return statement

{-
    ghci> accessCandidateMaybe 1
    Just "failed"
    ghci> accessCandidateMaybe 2
    Just "failed"
    ghci> accessCandidateMaybe 3
    Just "passed"
    ghci> accessCandidateMaybe 4
    Nothing

The code above is essentially identical. The only difference is we've assigned the
variable with <- using a lookup rather than reading from stdin. This is because the
Monad type class and the do-notation have abstracted away the context we're working
in. Because of this, we can think about solving all problems in a context in the
same way and we can start to design programs that work in any context.
-}

{- 31.2.4 The List context - processing a list of candidates

Because List is a Monad, we can convert the function into one that handles lists.
-}
candidates = [candidate1, candidate2, candidate3]

accessCandidateList :: [Candidate] -> [String]
accessCandidateList candidates = do
    candidate <- candidates
    let passed = viable candidate
    let statement = if passed then "passed" else "failed"
    return statement

{-
    ghci> accessCandidateList candidates
    ["failed","failed","passed"]

As we've seen, we haven't done much to change the core logic of this function.
Working with list by using the tools of the Monad type class, we can treat entire
lists as single values. If you didn't know Haskell, you would assume the body of
this function was for a single candidate value.

We could write the same logic using map:
-}

assessCandidates :: [Candidate] -> [String]
assessCandidates candidates = map (\x -> if x
 then "passed"
 else "failed") passed
 where passed = map viable candidates

{-
but this is a lot different to the code for the Maybe and IO contexts. We're forced
to think in terms of lists. More importantly, there is no way to generalise this
code for other types in a context.
-}

{- 31.2.5 Putting it all together and writing a monadic function

We've had to write three distinct functions for each context so far, but in this
section, we'll see a general solution that will solve all three contexts.

In order to make our function work for any context, we just need to change it's
type signature!
-}

accessCandidate :: Monad m => m Candidate -> m String
accessCandidate candidates = do
    candidate <- candidates
    let passed = viable candidate
    let statement = if passed then "passed" else "failed"
    return statement

{-
    ghci> accessCandidate readCandidate
    Enter candidate ID:
    1
    Enter code review grade:
    A
    Enter culture fit grade:
    B
    Enter education:
    PhD
    "passed"

    ghci> accessCandidate (Map.lookup 1 candidateDB)
    Just "failed"

    ghci> accessCandidate candidates
    ["failed","failed","passed"]
-}

{- Summary

It is important to understand the desugared monadic code as it can help
tremendously in debugging and understanding issues when working with Monads.

We saw how code written for IO using do-notation can be trivially rewritten for
Maybe types. This allows us to write more generalised code that will work on all
Monads.
-}

{- Q31.1

At the end of lesson 21, you saw the following program used to calculate the
cost of pizza:

   main :: IO ()
   main = do
    putStrLn "What is the size of pizza 1"
    size1 <- getLine
    putStrLn "What is the cost of pizza 1"
    cost1 <- getLine
    putStrLn "What is the size of pizza 2"
    size2 <- getLine
    putStrLn "What is the cost of pizza 2"
    cost2 <- getLine
    let pizza1 = (read size1, read cost1)
    let pizza2 = (read size2, read cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    putStrLn (describePizza betterPizza)
-}

-- From Lesson 21
data Pizza = Pizza { size :: Double, cost :: Double }
  deriving (Show, Eq)

areaGivenDiameter :: Double -> Double
areaGivenDiameter d = pi * (d / 2)^2

costPerArea :: Pizza -> Double
costPerArea (Pizza size cost) = cost / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if costP1 < costP2 then p1 else p2
  where
    costP1 = costPerArea p1
    costP2 = costPerArea p2

describePizza :: Pizza -> String
describePizza pizza = "The " ++ show (size pizza) ++ " pizza "
    ++ "is cheaper at " ++ show costSqInch ++ " per square inch"
  where
    costSqInch = costPerArea pizza

-- New stuff
readDouble :: IO Double
readDouble = read <$> getLine

readPizza :: Int -> IO Pizza
readPizza num = pure Pizza <*> sizePrompt <*> costPrompt
  where
    sizePrompt = putStrLn ("What is the size of pizza " ++ show num) >> readDouble
    costPrompt = putStrLn ("What is the cost of pizza " ++ show num) >> readDouble

main :: IO ()
main =
    pure comparePizzas <*> (readPizza 1) <*> (readPizza 2)
        >>= (\betterPizza -> putStrLn (describePizza betterPizza))

-- This was pretty tough without completely changing the flow of code. The answer
-- uses tons of nested lambdas to desugar the do-notation exactly:

mainSolution :: IO ()
mainSolution =
    putStrLn "What is the size of pizza 1" >>
    readDouble >>= (\size1 ->
        putStrLn "What is the cost of pizza 1" >>
        readDouble >>= (\cost1 ->
            putStrLn "What is the size of pizza 2" >>
            readDouble >>= (\size2 ->
               putStrLn "What is the cost of pizza 2" >>
               readDouble >>= (\cost2 ->
                   (\pizza1 ->
                       (\pizza2 ->
                           (\betterPizza ->
                               putStrLn (describePizza betterPizza)
                           )(comparePizzas pizza1 pizza2)
                       )(Pizza {size = size2, cost = cost2})
                   )(Pizza {size = size1, cost = cost1})
               )
            )
        )
    )

{-
    ghci> mainSolution
    What is the size of pizza 1
    8
    What is the cost of pizza 1
    10
    What is the size of pizza 2
    12
    What is the cost of pizza 2
    16
    The 12.0 pizza is cheaper at 0.1414710605261292 per square inch
-}

{- Q31.2
At the end of lesson 21 in unit 4, we first introduced the idea that do-notation
isn’t specific to IO. You ended up with this function for a Maybe type:

   maybeMain :: Maybe String
   maybeMain = do
    size1 <- Map.lookup 1 sizeData
    cost1 <- Map.lookup 1 costData
    size2 <- Map.lookup 2 sizeData
    cost2 <- Map.lookup 2 costData
    let pizza1 = (size1,cost1)
    let pizza2 = (size2,cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    return (describePizza betterPizza)

Rewrite this function so it works with the List type (don’t worry if the results
seem strange).
-}
pizza1 = Pizza {size = 8, cost = 10}
pizza2 = Pizza {size = 12, cost = 16}
pizza3 = Pizza {size = 10, cost = 12}
pizza4 = Pizza {size = 18, cost = 18}

pizzaList1 = [pizza1, pizza2]
pizzaList2 = [pizza3, pizza4]

listMain :: [Pizza] -> [Pizza] -> [String]
listMain pizzas1 pizzas2 = do
    pizza1 <- pizzas1
    pizza2 <- pizzas2
    let betterPizza = comparePizzas pizza1 pizza2
    return (describePizza betterPizza)

{-
    ghci> listMain pizzaList1 pizzaList2
    [
        "The 10.0 pizza is cheaper at 0.15278874536821951 per square inch",
        "The 18.0 pizza is cheaper at 7.07355302630646e-2 per square inch",
        "The 12.0 pizza is cheaper at 0.1414710605261292 per square inch",
        "The 18.0 pizza is cheaper at 7.07355302630646e-2 per square inch"
    ]
-}

{- Q31.3
Refactor the maybeMain function from the preceding exercise so that it works with
any Monad. You’ll need to change the type signature as well as remove the
type-specific parts from the body of the function.
-}

genericMain :: Monad m => m Pizza -> m Pizza -> m String
genericMain pizzas1 pizzas2 = do
    pizza1 <- pizzas1
    pizza2 <- pizzas2
    let betterPizza = comparePizzas pizza1 pizza2
    return (describePizza betterPizza)

{-
    ghci> genericMain pizzaList1 pizzaList2
    ["The 10.0 pizza is cheaper at 0.15278874536821951 per square inch",
     "The 18.0 pizza is cheaper at 7.07355302630646e-2 per square inch",
     "The 12.0 pizza is cheaper at 0.1414710605261292 per square inch",
     "The 18.0 pizza is cheaper at 7.07355302630646e-2 per square inch"]

    ghci> genericMain (Just pizza1) (Just pizza2)
    Just "The 12.0 pizza is cheaper at 0.1414710605261292 per square inch"

    ghci> genericMain (pure pizza1 :: IO Pizza) (pure pizza2 :: IO Pizza)
    "The 12.0 pizza is cheaper at 0.1414710605261292 per square inch"

-}
