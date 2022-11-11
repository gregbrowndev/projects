-- In this capstone project, we'll look at basic cryptographic messages by
-- writing a Cipher class.

-- As always, use GHCi to print results and test functions:

  -- $ stack ghci
  -- ghci> :load app/Unit2-Types/Lesson15-Capstone/Main.hs

-- Implementing a basic cipher with the RotN algorithm
-- 1. pass size of alphabet and letter to rotate
-- 2. divide size of alphabet into 2 using integer division
-- 3. rotate int-value of letter (fromEnum) half the size of the alphabet to new
--    int-value. Use modulus to avoid going out of bounds
-- 4. convert int value back to enum (toEnum)

data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN size c = toEnum rotation
  where halfSize = size `div` 2
        offset = fromEnum c + halfSize
        rotation = offset `mod` size

  -- ghci> rotN 4 L1
  -- L3
  -- ghci> rotN 4 L2
  -- L4
  -- ghci> rotN 4 L3
  -- L1
  -- ghci> rotN 4 L4
  -- L2

-- Use the Bounded type class to find largest value
largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)
-- Note, we usually assume the lowest bound is simply 0. Because of this, the
-- size of the Char set is largestCharNumber + 1

-- We can now write a Char-specific rotN function
rotChar :: Char -> Char
rotChar c = rotN size c
  where size = fromEnum (maxBound :: Char) + 1

-- Create an encoder of a list using map
fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder vals = map rot4l vals
  where size = fromEnum (maxBound :: FourLetterAlphabet) + 1
        rot4l = rotN size

message4l :: [FourLetterAlphabet]
message4l = [L1, L3, L4, L1, L1, L2]

  -- ghci> fourLetterAlphabetEncoder message4l
  -- [L3,L1,L2,L3,L3,L4]

-- Decoding our message
-- If the size of the alphabet is even, we can apply the encoder again to get
-- back to the original message. However, if it is odd, then we get the wrong
-- answer. Instead, we'll need some extra decoder logic
rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder size c = toEnum rotation
  where halfSize = size `div` 2
        offset = if even halfSize
                 then fromEnum c + halfSize
                 else fromEnum c + halfSize + 1
        rotation = offset `mod` size

-- Let's test on a 3 letter alphabet
data ThreeLetterAlphabet = Alpha | Beta | Kappa deriving (Show, Enum, Bounded)

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder vals = map rot3l vals
  where size = fromEnum (maxBound :: ThreeLetterAlphabet) + 1
        rot3l = rotN size

threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder vals = map rot3l vals
  where size = fromEnum (maxBound :: ThreeLetterAlphabet) + 1
        rot3l = rotNdecoder size

message3l = [Alpha, Alpha, Beta, Alpha, Kappa]

  -- ghci> message3l
  -- [Alpha,Alpha,Beta,Alpha,Kappa]
  -- ghci> threeLetterEncoder message3l
  -- [Beta,Beta,Kappa,Beta,Alpha]
  -- ghci> threeLetterDecoder (threeLetterEncoder message3l)
  -- [Alpha,Alpha,Beta,Alpha,Kappa]

-- If we used the encoder on the odd numbered alphabet, we'd get:

  -- ghci> threeLetterEncoder (threeLetterEncoder message3l)
  -- [Kappa,Kappa,Alpha,Kappa,Beta]

-- These are the final String encoder/decoder
rotEncoder :: String -> String
rotEncoder text = map rotChar text
  where size = fromEnum (maxBound :: Char) + 1
        rotChar = rotN size

rotDecoder :: String -> String
rotDecoder text = map rotChar text
  where size = fromEnum (maxBound :: Char) + 1
        rotChar = rotNdecoder size

-- Improving the cryptographic strength of our cipher with XOR
-- If we take a list of values to encrypt and XOR it with a list of random bits
-- (booleans or 0/1s), it will produce a new encoded list that is
-- indistinguishable from the random noise of the random bits list!

-- Create a XOR function (note Data.Bool provides an XOR function)
xorBool :: Bool -> Bool -> Bool
xorBool value1 value2 = (value1 || value2) && (not (value1 && value2))

xorPair :: (Bool, Bool) -> Bool
xorPair (value1, value2) = xorBool value1 value2

xor :: [Bool] -> [Bool] -> [Bool]
xor vals1 vals2 = map xorPair (zip vals1 vals2)

  -- ghci> xor [True, False, True, False] [False, True, True, False]
  -- [True,True,False,False]

-- String encoder
-- We want to write a string encoder. To this we need to convert each Char into
-- its enum int form. Then we need to convert this base 10 number into base 2
-- to give us a stream of bits. We can then use our xor function on the binary
-- representation of the string.

-- Bits type synonym will make the code more readable
type Bits = [Bool]

-- Write int to bits helper function (note apostrophe ')
-- to convert n to bits, recursively divide n by 2. If it has a remainder, add
-- 0, otherwise add 1
intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if (remainder == 0)
               then False : intToBits' nextVal
               else True : intToBits' nextVal
  where remainder = n `mod` 2
        nextVal = n `div` 2

-- Note, this function produces the bits in reverse! Additionally, the output
-- lists all have different lengths. If we convert 4, we'll get a three bits,
-- while `intToBits' maxBound` will return 63. We'll fix both in the final
-- function.

  -- ghci> intToBits' 4
  -- [False,False,True]
  -- ghci> intToBits' 5
  -- [True,False,True]
  -- ghci> intToBits' 6
  -- [False,True,True]

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
  where reversedBits = reverse (intToBits' n)
        missingBits = maxBits - (length reversedBits)
        leadingFalses = take missingBits (cycle [False])

  -- ghci> intToBits 4
  -- [False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,True,False,False]

-- we can make it more readable:

  -- ghci> map (\b -> fromEnum b) (intToBits 4)
  -- [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0]

-- Now let's write the function to convert a single char to bits
charToBits :: Char -> Bits
charToBits c = intToBits (fromEnum c)

-- We now have the tools to create more cryptographically secure messages.
-- We'll need to add a decoder to take a list of bits and convert it to an int
-- We can add 2^index for each bit that is True given its index in the list

-- To better understand this:
-- the binary 101 == 1*2^2 + 0*2^1 + 1*2^0 = 5

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\(_, i) -> 2^i) trueBits)
  where size = length bits
        indices = [size-1, size-2 .. 0]
        trueBits = filter (\x -> fst x == True) (zip bits indices)

-- Let's verify the function:

  -- ghci> bitsToInt [True, False, False]
  -- 4
  -- ghci> bitsToInt [True, False, True]
  -- 5
  -- ghci> bitsToInt (intToBits 32)
  -- 32

-- Note: there is a problem with this function in that it cannot handle negative
-- values. However, for our purposes for encoding chars it is fine as the Enum
-- value will always go from 0 to maxBound. We'll revisit this problem in lesson
-- 38!

-- Let's write a function to convert bits to char
bitsToChar :: Bits -> Char
bitsToChar bits =  toEnum (bitsToInt bits)

-- ghci> bitsToChar (charToBits 'g')
-- 'g'
-- ghci> bitsToChar (charToBits 't')
-- 't'

-- Implementing One-Time Pad
-- We now have all the tools to create an unbreakable one-time pad in contrast
-- to the weak ROT13 algo at the start of the capstone project. For one-time pad
-- we take a text and another text of sufficiently random characters that is at
-- least as long. This noise text is our cipher key. It was traditionally
-- written on a pad of paper, hence the term 'pad'.

myPad :: String
myPad = "Shhhhhhh"

myPlainText :: String
myPlainText = "Haskell"

-- we need to convert both the pad and plain text to bits and then xor them
-- together
applyOTP' :: String -> String -> [Bits]
applyOTP' pad text = map (\(p, t) -> p `xor` t) (zip padBits textBits)
  where padBits = map charToBits pad
        textBits = map charToBits text

applyOTP :: String -> String -> String
applyOTP pad text = map bitsToChar bitList
  where bitList = applyOTP' pad text

  -- ghci> applyOTP myPad myPlainText
  -- "\ESC\t\ESC\ETX\r\EOT\EOT"

-- As we can see, it's never a good idea to roll your own cryptography system.
-- There are repeated values in the output because our pad wasn't sufficiently
-- random. We can see that the 'l's in "Haskell" which were XORed with 'h' in
-- the pad have produced the same value, \EOT, in the output.

-- Remember that XOR provides a uniformly random output given the one of the
-- values being xorBool is uniformly random. If the pad used was random and an
-- attacker did not know the pad, the encrypted value would be uncrackable.

-- Also note, our OTP is symmetric so we can use it to decrypt messages too.
encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad  -- partial application

  -- ghci> encoderDecoder "book"
  -- "1\a\a\ETX"
  -- ghci> encoderDecoder "1\a\a\ETX"
  -- "book"


-- The Cipher class
class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

-- To support multiple cipher algorithms, we'll create new types as instances of
-- the Cipher class

-- The RotN algo from the start can be supported like so:
data Rot = Rot
instance Cipher Rot where
  encode Rot text = rotEncoder text
  decode Rot text = rotDecoder text

-- Note: the impl. of encode and decode above are using pattern matching to
-- match on the Rot literal type as the first arg to the function. I'm finding
-- it hard to get my head around why that is necessary but it becomes more
-- obvious looking at how you use it and comparing it to the OTP example

  -- ghci> encode Rot "Haskell"
  -- "\557128\557153\557171\557163\557157\557164\557164"
  -- ghci> decode Rot (encode Rot "Haskell")
  -- "Haskell"

-- For OTP, we need the extra pad argument
data OneTimePad = OTP String
instance Cipher OneTimePad where
  encode (OTP pad) text = applyOTP pad text
  decode (OTP pad) text = applyOTP pad text

  -- ghci> otp = OTP "Shhhhhhh"
  -- ghci> encode otp "Haskell"
  -- "\ESC\t\ESC\ETX\r\EOT\EOT"
  -- ghci> decode otp "\ESC\t\ESC\ETX\r\EOT\EOT"
  -- "Haskell"

-- Or using a better pad that allows arbitrary lengths (still not random though)

  -- ghci> otp = OTP (cycle [minBound .. maxBound])
  -- ghci> encode otp "Let's learn Haskell"
  -- "Ldv$w%jbi{d+Dl}du}~"
  -- ghci> decode otp "Ldv$w%jbi{d+Dl}du}~"
  -- "Let's learn Haskell"


-- Summary
-- In this lesson, we learned:
--  * Used basic type classes, Enum and Bounded, to build a generic rotN cipher
--  * Learned how to use XOR to encrypt a stream of Bools
--  * Used type synonym Bits to represent [Bool]
--  * Learned how different types work together to convert from Char to Bits
--    and Bits to Char.
--  * Created a powerful cryptographic tool, One-Time Pad
--  * Defined a generic Cipher type class as an interface for different cipher
--    implementations and implemented it for rotN and OTP for strings.
