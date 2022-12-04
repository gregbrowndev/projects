{-
In this program we'll use getContents to retrieve the user input as a list of
Chars.

Note: compile and run the program with:

  $ stack exec -- ghc -o .stack-work/sum_lazy ./app/Unit4-IO/Lesson22-Lazy-IO/sum_lazy.hs
  $ .stack-work/sum_lazy
-}

{-
main :: IO ()
main = do
  userInput <- getContents
  mapM_ print userInput
-}

{-
  $ .stack-work/sum_lazy
  hello
  'h'
  'e'
  'l'
  'l'
  'o'
  '\n'
  hi
  'h'
  'i'
  '\n'

Note: we have to use Ctrl + C to exit.

The interesting thing here is that we get the content printed out immediately.
Unlike in a strict (non-lazy) language, we'd expect to have to wait until the
user exits the program to see the final result display. However, because Haskell
handles lists lazily, we can process the text as soon as its entered and wait
for more inputs to be entered.
-}

-- Quick Check 22.3: Write a lazy I/O program to reverse the input
-- see reverse_lazy.hs

-- Use lines function to split the Char list into newlines by '\n'. Note, the
-- Data.List.Split module contains a more generic function splitOn that takes a
-- separator char.

toInts :: [Char] -> [Int]
toInts = map read . lines

main :: IO ()
main = do
  userInput <- getContents
  let numbers = toInts userInput
  let squares = map (^2) numbers
  print (sum squares)

{-
The above code is much cleaner than our original solution below!

  main :: IO ()
  main = do
    args <- getArgs
    let linesToRead = if length args > 0
                      then read (head args)
                      else 0 :: Int
    numbers <- replicateM linesToRead getLine
    let ints = map read numbers :: [Int]
    print (sum ints)
-}
