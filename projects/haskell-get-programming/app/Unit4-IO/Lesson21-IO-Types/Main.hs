import           System.Random

{- Lesson 21 - Introducing IO Types

After this lesson, we'll be able to:
  - Understand how Haskell handles I/O by using IO types
  - Use do-notation to perform I/O
  - Write pure programs that interact with the real world

In Haskell, any function that interacts with the real world must return an IO
type. The IO type makes it impossible to use values that have been tainted with
I/O in other pure function.
-}

mystery1 :: Int -> Int -> Int
mystery1 val1 val2 = (val1 + val2 + val3) ^ 2
  where val3 = 3

mystery2 :: Int -> Int -> IO Int
mystery2 val1 val2 = do
  putStrLn "Enter a number"
  val3Input <- getLine
  let val3 = read val3Input
  return ((val1 + val2 + val3) ^ 2)

{-
  ghci> mystery2 2 3
  Enter a number
  8
  169
-}

-- Revisiting a program from Unit 1
helloPerson :: String -> String
helloPerson name = "Hello " ++ name ++ "!"

main :: IO ()
main = do
  putStrLn "Hello, what's your name?"
  name <- getLine
  let statement = helloPerson name
  putStrLn statement

{-
When we first start Unit 1, the chances are the code above would have been
fairly readable and understandable. However, now we know more Haskell, it is
likely much less so! The main function looks like nothing we seen in the last
few units.

  - What is the IO () type?
  - Why is there a do after main?
  - Does putStrLn return a value?
  - Why are some variables assigned with <- and others with let?

IO, like the Maybe type, describes a context of their parameter (rather than a
container like List or Map). The context for the IO type is that the value has
come from an input/output operation. Common examples of this include reading
user input, printing to standard out, and reading a file.

The type IO () is the IO context with an empty tuple as its parameter. We could
also write Maybe (). However, this would be useless since it can only have two
values Just () and Nothing. Arguably Just () is Nothing. From this we can see
the main function returns nothing within the context of IO. The function
putStrLn simply returns IO ().


In the beginning of this book, we said that function must support three
properties:

  - It must take a value
  - It must return a value
  - Whenever the same argument is supplied, the same value must be returned
    (referential transparency)

Clearly, main breaks these rules as it doesn't return a meaningful value. It
simply performs an action. Because of this main is not a function, it is an
IO action!

The same logic follows that neither putStrLn nor getLine are functions but IO
actions:

  ghci> :i putStrLn
  putStrLn :: String -> IO ()  -- returns IO ()
  ghci> :i getLine             -- takes no arguments
  getLine :: IO String
-}

minDie :: Int
minDie = 1

maxDie :: Int
maxDie = 6

roll :: IO ()
roll = do
  dieRoll <- randomRIO (minDie,maxDie)
  putStrLn (show dieRoll)

{-
We can use pattern matching to extract the value from a Maybe type because it
can only have two values Just or Nothing. However, with IO, anything can go
wrong so the value must always stay in the context of IO. To work with this, we
need a way to perform a sequence of computations within the context of IO. That
is the purpose of the special do-notation that we see above.

The do-notation allows us to treat IO types as if they are regular types. This
is also the reason why some variables use <- and others use let. The <- syntax
allows us to treat `IO a` as just type `a`, while let is used to create
variables that aren't IO types.

Note: the code above

  name <- getLine
  let statement = helloPerson name
  putStrLn statement

cannot be shortened to

  let statement = helloPerson getLine

because getLine returns IO String.
-}
