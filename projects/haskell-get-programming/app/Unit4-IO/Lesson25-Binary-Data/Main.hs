{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as E
import qualified Data.Text.IO          as TIO

{- Lesson 25: Working with Files

After this lesson, I'll be able to:
  - Use the ByteString type to efficiently work with binary data
  - Treat ByteString as reguler ASCII strings by using ByteString.Char8
  - Glitch JPEG images by using Haskell
  - Work with binary Unicode data

In this lesson, we'll write a program to create glitch art. This is where
we deliberately corrupt binary data of an image to introduce visual
artifacts.

Even though ByteString is an array of bytes, we can always use ASCII
to represent strings of bytes. There are 256, or 2^8 (8 bits), ASCII
characters, so we can represent each byte as an ASCII character. If we
use the OverloadedStrings extension, we can use literal ASCII strings to
represent vectors of bytes.
-}

sampleBytes :: B.ByteString
sampleBytes = "Hello!"

{-
If we try to unpack the bytestring into a String, we'll get an error

    sampleString :: String
    sampleString = B.unpack sampleBytes

We can see the signature of B.unpack converts the ByteString into a list
of bytes (of type Word8):

    ghci> :i B.unpack
    B.unpack :: B.ByteString -> [GHC.Word.Word8]

Instead we have to use Data.ByteString.Char8.unpack:

    ghci> :info BC.unpack
    BC.unpack :: BC.ByteString -> [Char]

This works just like Data.List.unpack. One thing to note is that
B.unpack signature has now changed!

    ghci> :info B.unpack
    B.unpack :: BC.ByteString -> [GHC.Word.Word8]

Note the first arg is now BC.ByteString. This means from here on out, we
can use ByteStrings as plain ASCII text.

Now we have covered the basics of ByteString, we'll write a program in
glitcher.hs to create glitch art.
-}


{-
You need to be careful when using ByteString, ByteString.Char8, and Unicode
data. It is easy to lose our Unicode data is we do not encode it properly,
like in the code below:
-}

nagarjunaText :: T.Text
nagarjunaText = "नागर्जुनॅ"

nagarjunaB :: B.ByteString
nagarjunaB = (BC.pack . T.unpack) nagarjunaText

{-
If we try to reverse these operations to get back our Text, we'll
get the wrong answer

    ghci> TIO.putStrLn ((T.pack . BC.unpack) nagarjunaB)
    (>0MA(E

To solve this issue we need to use Data.Text.Encoding. This module has
two essential functions:

    E.encodeUtf8 :: T.Text -> BC.ByteString
    E.decodeUtf8 :: BC.ByteString -> T.Text
-}

nagarjunaSafe :: B.ByteString
nagarjunaSafe = E.encodeUtf8 nagarjunaText

{-
    ghci> TIO.putStrLn (E.decodeUtf8 nagarjunaSafe)
    नागर्जुनॅ

To be safe, never use the convenience of Data.ByteString.Char8 if you're working
with data that may contain Unicode. Instead, stick to regular ByteString, Text, and
Text.Encoding.

In this lesson, we saw how ByteString allows us to treat raw binary data the same
way as ordinary strings. This can greatly simplify how to write programs that
manipulate binary data. However, it is crucial to remember not to mix single-byte
representations of binary data (Char8) with Unicode text.
-}
