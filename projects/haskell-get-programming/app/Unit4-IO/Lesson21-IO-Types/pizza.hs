import qualified Data.Map as Map

data Pizza = Pizza { size :: Double, cost :: Double }

main :: IO ()
main = do
  putStrLn "What is the size of pizza 1?"
  pizza1Size <- getLine
  putStrLn "What is the cost of pizza 1?"
  pizza1Cost <- getLine
  putStrLn "What is the size of pizza 2?"
  pizza2Size <- getLine
  putStrLn "What is the cost of pizza 2?"
  pizza2Cost <- getLine
  let pizza1 = Pizza (read pizza1Size) (read pizza1Cost)
  let pizza2 = Pizza (read pizza2Size) (read pizza2Cost)
  let betterPizza = comparePizzas pizza1 pizza2
  putStrLn (describePizza betterPizza)


areaGivenDiameter :: Double -> Double
areaGivenDiameter d = pi * (d / 2)^2

costPerArea :: Pizza -> Double
costPerArea (Pizza size cost) = cost / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if costP1 < costP2
                      then p1
                      else p2
  where costP1 = costPerArea p1
        costP2 = costPerArea p2

describePizza :: Pizza -> String
describePizza pizza = "The " ++ show (size pizza) ++ " pizza " ++
                             "is cheaper at " ++ show costSqInch ++
                             " per square inch"
  where costSqInch = costPerArea pizza

{-
  ghci> main
  What is the size of pizza 1?
  8
  What is the cost of pizza 1?
  10
  What is the size of pizza 2?
  12
  What is the cost of pizza 2?
  16
  The 12.0 pizza is cheaper at 0.1414710605261292 per square inch

The do-notation works with IO because it is a member of the Monad type class.
We can also work with Maybe in the same way!
-}

pizzaData :: Map.Map Int Pizza
pizzaData = Map.fromList [
  (1, Pizza {size =  8.0, cost = 10.0}),
  (2, Pizza {size = 12.0, cost = 16.0})
 ]

maybeMain :: Maybe String
maybeMain = do
  pizza1Maybe <- Map.lookup 1 pizzaData
  pizza2Maybe <- Map.lookup 2 pizzaData
  let pizza1 = pizza1Maybe
  let pizza2 = pizza2Maybe
  let betterPizza = comparePizzas pizza1 pizza2
  return (describePizza betterPizza)

{-
  ghci> maybeMain
  Just "The 12.0 pizza is cheaper at 0.1414710605261292 per square inch"

The new thing here is the `return (describePizza betterPizza)` function which
takes a value of a type and puts it back into the context of the do-notation.

The Monad type class allows you to write general programs that work in a wide
range of contexts, i.e. we can use the same core functions in a program that
must deal with missing values, values from IO, futures, etc.
-}

{-
Questions
  - Q21.1: Translate the code in listing 21.1 into do-notation using Maybe
-}

helloPerson :: String -> String
helloPerson name = "Hello " ++ name ++ "!"

nameMap :: Map.Map Int String
nameMap = Map.fromList [(1, "Greg"), (2, "Kieran")]

mainPerson :: Maybe String
mainPerson = do
  name <- Map.lookup 1 nameMap
  let statement = helloPerson name
  return statement

{-
  - Q21.2: Write a program that asks for a number and returns the nth Fibonacci
    number.
-}

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = fib (n-2) + fib (n-1)

mainFib :: IO ()
mainFib = do
  putStrLn "Enter the nth Fibonacci number to generate"
  n <- getLine
  let fibN = fib (read n)
  putStrLn (show fibN)
