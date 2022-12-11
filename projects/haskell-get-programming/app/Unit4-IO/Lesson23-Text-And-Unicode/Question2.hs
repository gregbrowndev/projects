{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text.Lazy    as T
import qualified Data.Text.Lazy.IO as TIO

{-
Q23.2: Use Data.Text.Lazy and Data.Text.Lazy.IO to rewrite the lazy I/O section from
       lesson 22 by using Text.
-}
toInts :: T.Text -> [Int]
toInts = map (read . T.unpack) . T.lines

main :: IO ()
main = do
  userInput <- TIO.getContents
  let numbers = toInts userInput
--  print (sum numbers)
  TIO.putStrLn ((T.pack . show. sum) numbers)
