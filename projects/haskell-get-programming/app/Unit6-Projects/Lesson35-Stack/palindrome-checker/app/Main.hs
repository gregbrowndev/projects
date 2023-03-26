module Main (main) where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           Palindrome

main :: IO ()
main = do
    print ("Enter a word" :: T.Text)
    text <- TIO.getLine
    let response = if isPalindrome text
                   then text <> " is a palindrome"
                   else text <> " is not a palindrome"
    print response
