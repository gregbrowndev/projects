import           Data.Char                 (isPunctuation, isSpace)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO

import           Test.QuickCheck
import           Test.QuickCheck.Instances

import           Palindrome

{-
There are testing libraries for Haskell but for now we'll write our own simple
tools.

Run these tests with:

    $ stack test
-}

assert :: Bool -> T.Text -> T.Text -> IO ()
assert test passStatement failStatement = if test
                                          then TIO.putStrLn passStatement
                                          else TIO.putStrLn failStatement

testPalindrome :: T.Text -> Bool -> IO ()
testPalindrome text expected = assert ((isPalindrome text) == expected) passStatement failStatement
  where
    passStatement = mconcat ["passed '", text, "'"]
    failStatement = mconcat ["FAIL '", text, "'"]

mainUnit :: IO ()
mainUnit = do
    putStrLn "Running tests..."
    testPalindrome "racecar" True
    testPalindrome "racecar!" True
    testPalindrome "cat" False

{-
In order to get good test coverage over the possible inputs, we have to come
up with many examples to test. This is called example-based testing.

In test/PalindromeSpec.hs, we'll use QuickCheck to write property-based tests
that automate the generation of examples and focuses testing around properties
that we express in our test code.

Make sure to add QuickCheck to our dependencies in package.yaml.

What we really care about in isPalindrome is that it is punctuation invariant,
i.e. we don't care if it contains punctuation. We can write a function to
express this property, prop_punctuationInvariant.
-}
prop_punctuationInvariant :: T.Text -> Bool
prop_punctuationInvariant text =
    isPalindrome text == isPalindrome noPuncText
  where
    noPuncText = T.filter (not . isPunctuation) text

-- QC: write a property that shows isPalindrome is invariant to reversing
--     the input
prop_reverseInvariant :: T.Text -> Bool
prop_reverseInvariant text =
    isPalindrome text == isPalindrome (T.reverse text)

prop_whitespaceInvariant :: T.Text -> Bool
prop_whitespaceInvariant text =
    isPalindrome text == isPalindrome (T.filter (not . isSpace) text)

prop_capsInvariant :: T.Text -> Bool
prop_capsInvariant text =
    isPalindrome text == isPalindrome (T.toLower text)

{-
Note: in order to make QuickCheck work with a property that takes a Data.Text
as input, we need to make Data.Text an instance of the Arbitrary type class

    instance Arbitrary T.Text where
      arbitrary = T.pack <$> arbitrary

Note: we can also install the quickcheck-instances library to do the same thing
-}

-- A main entrypoint to run QuickCheck
main :: IO ()
main = do
    -- quickCheck prop_punctuationInvariant  -- lets change the number of tests
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_punctuationInvariant
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_whitespaceInvariant
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_capsInvariant
    putStrLn "done"
