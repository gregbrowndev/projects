{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

-- The text below can be copied from https://gist.github.com/willkurt/4bced09adc2ff9e7ee366b7ad681cac6

-- The Sanskrit word for dharma meaning duty, cosmic order, divine justice...
dharma :: T.Text
dharma = "धर्म"

-- An excerpt from the Bhavagad Gita.
bgText :: T.Text
bgText = "श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्। स्वधर्मे निधनं श्रेयः परधर्मो भयावहः"

-- Our goal is to highlight the word dhamra in bgText.

{-
Note: unlike English, the words spoken in Sanskrit that naturally combine together are
joined together in text. This means we'll have to split on the word we're looking for rather
that split by whitespace.
-}

highlight :: T.Text -> T.Text -> T.Text
highlight word text = T.intercalate highlightedWord parts
  where parts = T.splitOn word text
        highlightedWord = mconcat ["{", word, "}"]

{-
We need to write our main function to print out the highlighted text. However, so far
we've only worked with IO String. We need to use putStrLn for for Text... To do this
we need to use the Data.Text.IO module, so we can work with essentially IO Text.
-}
main :: IO ()
main = do
  TIO.putStrLn (highlight dharma bgText)

{-
  ghci> main
  श्रेयान्स्व{धर्म}ो विगुणः पर{धर्म}ात्स्वनुष्ठितात्। स्व{धर्म}े निधनं श्रेयः पर{धर्म}ो भयावहः
-}
