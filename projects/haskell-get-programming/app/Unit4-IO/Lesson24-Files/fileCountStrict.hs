{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import           System.Environment
import           System.IO


main :: IO ()
main = do
  args <- getArgs
  let filePath = head args
  input <- TIO.readFile filePath
  let summary = (countsText . getCounts) input
  TIO.appendFile "app/Unit4-IO/Lesson24-Files/stats.dat" (mconcat [T.pack filePath, " ", summary, "\n"])
  TIO.putStrLn summary


getCounts :: T.Text -> (Int, Int, Int)
getCounts input = (charCount, wordCount, lineCount)
  where charCount = T.length input
        wordCount = (length . T.words) input
        lineCount = (length . T.lines) input


countsText :: (Int, Int, Int) -> T.Text
countsText (cc, wc, lc) = T.pack $ unwords ["chars: ", show cc, " words: ", show wc, " lines: ", show lc]

{-
  ghci> :l app/Unit4-IO/Lesson24-Files/fileCountStrict.hs
  ghci> :main "app/Unit4-IO/Lesson24-Files/stats.dat"
  chars:  150  words:  14  lines:  2
-}
