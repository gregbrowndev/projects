{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T

{- Lesson 23 - Working with Text and Unicode

After this lesson, I'll be able to:

  - Use the Text type for more-efficient text processing
  - Change Haskell's behaviour with language extensions
  - Program by using common text functions
  - Use Text to properly handle Unicode text

So far, we've made heavy use of the String type and we've seen that we can treat
an I/O stream as a lazy list of type Char. String has been useful up to this
point but it is very inefficient. A String is simply a list of Chars. However,
the problem is implementing a string as a linked list is needlessly expensive
in both time and space. In this lesson, we'll learn about a better type for
dealing with strings, Text.

The Text type is implemented as an array under the hood. This makes string
operations faster and much more memory-efficient. Another big difference is that
Text doesn't use lazy-evaluation. Lazy evaluation can deal to performance
headaches. If you do need lazy evaluation, use Data.Text.Lazy.

The Data.Text module has two functions, pack and unpack, that build Text from
a String and visa versa.

  T.pack :: String -> T.Text
  T.unpack :: T.Text -> String
-}

firstWord :: String
firstWord = "pessimism"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

-- Note: the conversion isn't cheap, so avoid converting between String and Text
-- unnecessarily

{-
Annoyingly, the following code throws an error

  myWord :: T.Text
  myWord = "dog"

because Haskell cannot convert the literal into the type you want, even though
it is happy to do so with ints, e.g.

  myNum1 :: Int
  myNum1 = 3

  myNum2 :: Integer
  myNum2 = 3

  myNum3 :: Double
  myNum3 = 3

We can work around this using language extensions to alter the way Haskell works
itself. We can use the OverloadedStrings extension to solve the problem.

You can add language extensions as compiler flags. However, the preferred
approach is to use a LANGUAGE pragma.

  {-# LANGUAGE <Extension Name> #-}
-}

myWord :: T.Text
myWord = "dog"

main :: IO ()
main = do
  print myWord

{-
Other useful language extensions:

  - ViewPatterns - Allows for more-sophisticated pattern matching
  - TemplateHaskell - Provides tools for Haskell metaprogramming
  - DuplicateRecordFields - Solves the annoying problem from lesson 16, where
    using the same field name for different types using record syntax causes a
    conflict.
  - NoImplicitPrelude - As mentioned, some Haskell programmers prefer to use a
    custom Prelude. This language extension allows you to not use the default
    Prelude.

All of the useful functions we've seen for working with String are reproduced
in the Data.Text module, e.g.
-}

sampleInput :: T.Text
sampleInput = "this\nis\ninput"

{-
  ghci> T.lines sampleInput
  ["this","is","input"]

Here are some other useful functions in Data.Text:

  - T.words - like lines but works for any whitespace character
  - T.splitOn - like Data.List.Split. Fortunately, this is included in Data.Text
    so no other import is required
  - unwords - the inverse of words (same as ' '.join in Python)
  - unlines - the inverse of lines (same as '\n'.join in Python)
  - intercalate - the inverse of splitOn

The exception to the rule is the ++ operator that we use to join two strings together.
Unfortunately, this doesn't work for Text. However, remember that Text is a Monoid! The
idiomatic way to combine Text together is to use the <> operator or mconcat.

  ghci> {-# LANGUAGE OverloadedStrings #-}
  ghci> import qualified Data.Text as T
  ghci> import Data.Semigroup

  ghci> combineTextMonoid = mconcat ["some", " ", "text"]
  ghci> combineTextMonoid
  "some text"

  ghci> combineTextSemigroup = "some" <> " " <> "text"
  ghci> combineTextSemigroup
  "some text"

Note: String is also an instance of Monoid and Semigroup so they can be combined in the same way.
-}

{- Handling Unicode in Haskell with Text

Text handles Unicode seamlessly. Let's take a look at an application to highlight words in a
text. See bg_highlight.hs.
-}
