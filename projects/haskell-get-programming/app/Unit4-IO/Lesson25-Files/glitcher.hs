{-# LANGUAGE OverloadedStrings #-}
import System.Environment(getArgs)
import System.FilePath(splitFileName)
import System.Random(randomRIO)
import Control.Monad(foldM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

{-
Here’s the basic structure of the program:
    1. Take a filename argument from the user.
    2. Read in the binary data for the image file.
    3. Randomly alter bytes in the image data.
    4. Write a new file containing the glitched image.
-}

main :: IO ()
main = do
    args <- getArgs
    let filePath = head args
    imageFile <- BC.readFile filePath
--    glitched <- return imageFile  -- stub impl.
--    glitched <- randomReplaceByte imageFile
--    glitched <- randomSortSection imageFile
    glitched <- foldM (\bytes func -> func bytes) imageFile [
                        randomReplaceByte,
                        randomSortSection,
                        randomReverseBytes,
                        randomReplaceByte,
                        randomSortSection,
                        randomReverseBytes,
                        randomReplaceByte
                     ]
    let (directory, fileName) = splitFileName filePath
    let glitchedFilePath = mconcat [directory, "/glitched_", fileName]
    BC.writeFile glitchedFilePath glitched
    print "all done"

{-
Run the program from ghci with:

    ghci> :main app/Unit4-IO/Lesson25-Files/HP_Lovecraft.jpg

To corrupt the image, we'll first try to pick a random byte and
replace it with another random byte. Using random requires an IO action.
It is best to separate as much code from IO actions as possible as your
non-IO code is pure and predictable.
-}

intToChar :: Int -> Char
intToChar int = toEnum safeInt
    where safeInt = int `mod` 255

intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

-- Pure version of our goal
replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc charVal bytes = mconcat [before, newChar, after]
    where (before, rest) = BC.splitAt loc bytes
          after = BC.drop 1 rest
          newChar = intToBC charVal

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
    let bytesLength = BC.length bytes
    location <- randomRIO (1, bytesLength)
    charVal <- randomRIO (0, 255)
    return (replaceByte location charVal bytes)

{-
Changing a single byte has an underwhelming effect. We'll need to try
something else to see a more dramatic effect.

Another common approach is to get a chunk of bytes, sort them, and then
put everything back together again.
-}

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before, changed, after]
    where (before, rest) = BC.splitAt start bytes
          (target, after) = BC.splitAt size rest
          changed = BC.reverse (BC.sort target)

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
    let sectionSize = 25
    let bytesLength = BC.length bytes
    start <- randomRIO (0, bytesLength - sectionSize)
    return (sortSection start sectionSize bytes)

{- Q25.2: Add another technique, randomReverseBytes -}
reverseBytes :: Int -> Int -> BC.ByteString -> BC.ByteString
reverseBytes start size bytes = mconcat [before, changed, after]
    where (before, rest) = BC.splitAt start bytes
          (target, after) = BC.splitAt size rest
          changed = BC.reverse target

randomReverseBytes :: BC.ByteString -> IO BC.ByteString
randomReverseBytes bytes = do
    let sectionSize = 25
    let bytesLength = BC.length bytes
    start <- randomRIO (0, bytesLength - sectionSize)
    return (reverseBytes start sectionSize bytes)

{-
Now we have a cooler effect!

Next we'll look at chaining together IO actions with foldM.
Note: it would be valid but cumbersome to write:

main = do
    ...
    glitched1 <- randomReplaceByte imageFile
    glitched2 <- randomSortSection glitched1
    glitched3 <- randomReplaceByte glitched2
    ...

Instead, we can use foldM from Control.Monad. Just as mapM generalised
map to monads, foldM does the same for folding. With foldM, we can take
the original imageFile, a list of 10 IO actions, and a simple lambda to
apply these actions.
-}
