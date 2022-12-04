{-
Questions
  Q22.2: Write a program that allows a user to select a number between 1 and 5
  and then prints a famous quote. After printing the quote, the program asks
  the user if they would like another.
-}

data Command = PrintQuote Int | Exit

makePrintQuote :: Int -> Command
makePrintQuote i = PrintQuote ((i - 1) `mod` 5)

quotes :: [String]
quotes = [
    "quote 1",
    "quote 2",
    "quote 3",
    "quote 4",
    "quote 5"
  ]

parseInputs :: [Char] -> [Command]
parseInputs = map parseInput . lines

parseInput :: String -> Command
parseInput "n" = Exit
parseInput s   = makePrintQuote (read s)

processCommand :: Command -> String
processCommand (PrintQuote n) = quotes !! n
processCommand Exit           = "Exiting..."

-- Interesting that using recursion allows the output to be printed immediately,
-- whereas the code above only prints when the program exits
processCommands :: [Command] -> [String]
processCommands [] = []
processCommands (Exit:xs) = []
processCommands ((PrintQuote i):xs) = quote : (processCommands xs)
  where quote = quotes !! i

main :: IO ()
main = do
  putStrLn "Enter a number between 1 and 5"
  inputs <- getContents
  let commands = parseInputs inputs
  -- program 1 - this code doesn't work properly
--  let outputs = mapM processCommand commands
--  mapM_ putStrLn outputs
  -- program 2
  let outputs = processCommands commands
  mapM_ putStrLn outputs
