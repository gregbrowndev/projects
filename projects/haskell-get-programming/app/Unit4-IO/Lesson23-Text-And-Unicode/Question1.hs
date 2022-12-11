{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

{-
Q23.1: Rewrite the hello_world.hs program from lesson 21 to use Text instead of String
-}
helloPerson :: T.Text -> T.Text
helloPerson name = mconcat ["Hello ", name, "!"]

main :: IO ()
main = do
  putStrLn "Hello! What's your name?"
  name <- TIO.getLine
  let statement = helloPerson name
  TIO.putStrLn statement
