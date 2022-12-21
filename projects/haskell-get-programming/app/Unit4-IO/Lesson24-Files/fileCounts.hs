import           System.Environment
import           System.IO

{-
Note: it is much easier to call main from GHCi rather than have to compile this module!
There are a number of approaches:

  ghci> :set args foo bar
  ghci> main

  ghci> :main foo bar

  ghci> import System.Environment
  ghci> withArgs ["foo", "bar"] main
-}

main :: IO ()
main = do
  args <- getArgs
  let filePath = head args
  fileIn <- readFile filePath
  let summary = (countsText . getCounts) fileIn
  appendFile "app/Unit4-IO/Lesson24-Files/stats.dat" (mconcat [filePath, " ", summary, "\n"])
  putStrLn summary


getCounts :: String -> (Int, Int, Int)
getCounts input = (charCount, wordCount, lineCount)
  where charCount = length input
        wordCount = (length . words) input
        lineCount = (length . lines) input


countsText :: (Int, Int, Int) -> String
countsText (cc, wc, lc) = unwords ["chars: ", show cc, " words: ", show wc, " lines: ", show lc]
-- Note: we use unwords instead of String's (List's) more idiomatic ++ operator because as we
-- discussed in lesson 23, Text doesn't support ++. This will make the code A LOT easier to
-- refactor if we ever want to change to using Text.

{-
Let's run this program:

  ghci> :main "app/Unit4-IO/Lesson24-Files/fileCounts.hs"
  chars:  919  words:  141  lines:  35
-}

{-
We've left out a lot of checks for brevity. However, one interesting problem you might not
have thought of is what happens if we run the program on the stats.dat file as input?

  ghci> :main "app/Unit4-IO/Lesson24-Files/stats.dat"
  *** Exception: app/Unit4-IO/Lesson24-Files/stats.dat: openFile: resource busy (file is locked)

The problem is readFile doesn't close the file handle, so you cannot append to the file.
We need to revise the main function to close the file:
-}

mainRevised :: IO ()
mainRevised = do
  args <- getArgs
  let filePath = head args
  fileIn <- openFile filePath ReadMode
  input <- hGetContents fileIn
  hClose fileIn
  let summary = (countsText . getCounts) input
  appendFile "app/Unit4-IO/Lesson24-Files/stats.dat" (mconcat [filePath, " ", summary, "\n"])
  putStrLn summary

{-
However, now this code throws a runtime error:

  ghci> :set args "app/Unit4-IO/Lesson24-Files/stats.dat"
  ghci> mainRevised
  *** Exception: app/Unit4-IO/Lesson24-Files/stats.dat: hGetContents: illegal operation (delayed read on closed handle)

The problem is lazy evaluation. Since input isn't used until summary, no code is yet evaluated by the time we close
the file handle. Then when it is evaluated we've already closed the file! However, the problem continues, summary
isn't used until we call appendFile. Since appendFile is an IO action, it forces evaluation of summary, and input, but
then we'd be back at the same problem again as we are trying to append to the file which hasn't been closed yet.

The solution is to force evaluation of summary which we can do with putStrLn.
-}

mainRevised2 :: IO ()
mainRevised2 = do
  args <- getArgs
  let filePath = head args
  fileIn <- openFile filePath ReadMode
  input <- hGetContents fileIn
  let summary = (countsText . getCounts) input
  putStrLn summary
  hClose fileIn
  appendFile "app/Unit4-IO/Lesson24-Files/stats.dat" (mconcat [filePath, " ", summary, "\n"])

{-
  ghci> :set args "app/Unit4-IO/Lesson24-Files/stats.dat"
  ghci> mainRevised2
  chars:  79  words:  7  lines:  1

This serves as a warning that lazy evaluation can lead to some nasty bugs! If we rewrite the program
using Text, which is a strict type, it will solve the problem. Let's do that in fileCountStrict.hs.
-}
