import           Control.Monad
import           System.Environment

{-
main :: IO ()
main = do
  args <- getArgs  -- from System.Environment
  mapM_ putStrLn args
-}

{-
In the program above, we want to print each arg to stdout. We cannot use `map`
because args isn't an ordinary list and putStrLn isn't an ordinary function.
Instead, we need to use a special version of map, mapM, that operates on Lists
in the context of IO (or any member of the Monad type class).

If we compiled the program with mapM, we would see an error relating to the fact
that mapM returns a new list but our program is typed `IO ()`. We need to use
another version of mapM that throws away the result: mapM_.

Run the program with:

  $ stack exec -- ghc -o .stack-work/sum ./app/Unit4-IO/Lesson22-Lazy-IO/sum.hs
  $ ./.stack-work/sum 2 3 4 5
  2
  3
  4
  5

-}

-- Quick Check 22.1: Write a program that calls getLine three times
mainMapM :: IO ()
mainMapM = do
  inputs <- mapM (\_ -> getLine) [1..3]
  mapM_ putStrLn inputs


-- Let's update the main IO action to use the args
{-
main :: IO ()
main = do
  args <- getArgs
  let linesToRead = if length args > 0
                    then read (head args)
                    else 0 :: Int
  print linesToReads
-}

-- Note: the print function is `(putStrLn . show)`

{-
Now we know how many lines we need, we need to repeatably call getLine. Haskell
has a useful function, replicateM, that takes a value for the number of times
to repeat and an IO action to repeat. This needs to be imported from
Control.Monad. We also need to convert all the lines into ints.
-}

main :: IO ()
main = do
  args <- getArgs
  let linesToRead = if length args > 0
                    then read (head args)
                    else 0 :: Int
  numbers <- replicateM linesToRead getLine
  let ints = map read numbers :: [Int]
  print (sum ints)

{-
  $ stack exec -- ghc -o .stack-work/sum ./app/Unit4-IO/Lesson22-Lazy-IO/sum.hs
  $ .stack-work/sum 3
    5
    7
    3
    15
-}

{-
Summary of functions for iterating in an IO context

  mapM - Takes an IO action and a regular list, performing the action on each
         item in the list, and returning a list in the IO context
  mapM_ - Same as mapM but it throws away the values (note the underscore)
  replicateM - Takes an IO action, an Int n, and then repeats the IO action n
               times, returning the results in an IO list
  replicateM_ - Same as replicateM but it throws away the results


While this program worked fine it had a few issues. First, it required the user
to input the number of lines they want to input ahead of time. What if users
are keeping a running tally of visitors to a museum, or piping the output of
another program into this one? Another problem is that a lot of our logic is
coupled to the IO context. We haven't done a great job of separating out the
core logic from the IO. The root issue is that we are treating the IO data as
a sequence of values to deal with immediately. An alternative approach is to
treat the IO as a stream of data just like any other list in Haskell. Rather
than think of each piece of data as a discrete user interaction, we can treat
the entire interaction as a list of characters coming from the user.

Next, we we'll look at how much easier this is if we treat the input as a
list of Chars and use lazy evaluation. We'll do this in sum_lazy.hs.
-}
