{- Lesson 24 - Working with Files

In this lesson, we'll
  - Work with file handles in Haskell
  - Read from and write to files
  - Understand limitations of lazy evaluation for I/O
-}
import           System.IO

{-
To open a file we use the openFile function from System.IO:

  openFile :: FilePath -> IOMode -> IO Handle

We can check :info to find out more about these arguments:

  ghci> :info FilePath
  type FilePath = String
  ghci> :info IOMode
  data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode


-}

-- assuming we run this code from the project root
simpleMain :: IO ()
simpleMain = do
  helloFile <- openFile "app/Unit4-IO/Lesson24-Files/hello.txt" ReadMode
  hasLine <- hIsEOF helloFile
  firstLine <- if not hasLine
               then hGetLine helloFile
               else return "empty"
  putStrLn "done!"

{-
Interestingly, putStrLn and getLines are special cases of the more generic
functions hPutStrLn and hGetLines which both take a Handle as an arg.
putStrLn passes stdout as the Handle and getLines passes stdin, respectively.

Let's look at the signatures of a few useful functions:

  readFile :: FilePath -> IO String
  writeFile :: FilePath -> String -> IO ()
  appendFile :: FilePath -> String -> IO ()

To see these in actions, we'll create a program called fileCounts.hs.
-}
