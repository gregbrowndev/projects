{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import qualified Palindrome

{- Lesson 34: Organsing Haskell Code with Modules

After this lesson, we'll be able to:

    - Understand the main module implicitly used when you create a program
    - Create namespaces for your functions by using modules
    - Separate programs into multiple files
    - Selectively import functions from modules

In this lesson, we'll write a palindrome tester and organise the code into modules.
Ideally, we'll like the main IO action in a separate file from the functions that
do the business logic of the palindrome. This makes your program easier to extend
and organise into layers. We'll start by writing all the code in this file and then
refactor into two files.
-}

{-
Let's write a better version of head:

    head :: [a] -> a
    head (x:_) = x
    head []    = errorEmptyList "head"

The standard version throws errors, we can write one that uses Monoid to return
an empty list:
-}
head :: Monoid a => [a] -> a
head (x:xs) = x
head []     = mempty

example :: [[Int]]
example = []

{-
This code compiles but if you try to use head, you will get an ambiguous error.
To fix this we need to make this file a module! (It seems we need a main function
to appease the compiler.) Now we can call our head function:

    ghci> Main.head example
    []

    ghci> Prelude.head example
    *** Exception: Prelude.head: empty list
-}


{- 34.2.1 Creating the Main module

Note: I've refactored Main.hs to put the palindrome logic in its own file
Palindrome.hs.

We should be able to compile this file using:

    # compile and run executable ./Main
    $ stack exec -- ghc app/Unit6-Projects/Lesson34-Modules/Main.hs

    # or run it directly as a script
    $ stack exec -- runghc app/Unit6-Projects/Lesson34-Modules/Main.hs

That doesn't seem to work... instead, change directory into this one...

    $ cd app/Unit6-Projects/Lesson34-Modules
    $ stack ghci
    ghci> :l Main


Note: I got this working by updating the package.yaml file to include this
module as a source directory for the Main executable:

    executables:
      haskell-get-programming-exe:
        main:                Main.hs
        source-dirs:         app/Unit6-Projects/Lesson34-Modules
        ghc-options:
        - -threaded
        - -rtsopts
        - -with-rtsopts=-N

-}
main :: IO ()
main = do
    print "Enter a word"
    text <- TIO.getLine
    let response = if Palindrome.isPalindrome text
                   then text <> " is a palindrome"
                   else text <> " is not a palindrome"
    print response

{- Summary

In this lesson, we learned how to use modules to organise our programs
and how to fully qualify the namespace of a type to avoid name collisions.
-}

{- Q34.1

Recall that in unit 4 we mentioned that Data.Text is strongly preferred
over String for working with text data. Refactor this project to use
Data.Text instead of String (in both the Main and Palindrome modules).
-}

{- Q34.2

In unit 4, lesson 25, you wrote a program to “glitch” binary images.
Revisit that program and pull out all the code specific to glitching
images into its own Glitch module.
-}
