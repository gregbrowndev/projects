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
