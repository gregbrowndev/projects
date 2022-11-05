-- Int vs Integer
-- Int type is bounded with min and maximum values (that fit into a 64 bit word)
x :: Int
x = 2

overflow :: Int
overflow = x ^ 2000  -- 0

-- Integer type is unbounded and can  represent any whole number
y :: Integer
y = 2 ^ 2000  -- a very big number

-- Lists
values :: [Int]
values = [1, 2, 3]

-- a list of chars is the same as a string
letters :: [Char]
letters = ['h', 'e', 'l', 'l', 'o']
check = letters == "hello"  -- True

-- Tuples
-- tuples have a fixed length and can contain multiple types
ageAndHeight :: (Int, Int)
ageAndHeight = (25, 170)

fullName :: (String, String)
fullName = ("Bob", "Marley")

-- Function types
double :: Int -> Int
double n = n * 2

-- Casting
-- We cannot divide a whole number in half, instead we must convert it to a Double
-- Note: Haskell has no concept of implicit casting, instead we must use functions to convert from one type to another
half :: Int -> Double
half n = (fromIntegral n) / 2
-- Haskell is stricter and at the same type more flexible than Python or Ruby. It is stricter by prohibiting implicit
-- casting, while it is more flexible as literal numbers are treated polymorphically - their type is inferred from the
-- context by the compiler. For this reason, we don't need to convert the divisor '2' to a Double.

-- Converting to and from strings with show and read
valToString :: String
valToString = show 6

-- quick exercise
printDouble :: Int -> String
printDouble n = show (n * 2)

-- reading is trickier, how does Haskell know what type to convert it into?
valFromString = read "6"

-- Haskell is smart enough to infer the type from how the value is used later
-- in the code, e.g. valFromString becomes a Double!
q :: Double
q = valFromString / 2

-- We can also provide an explicit type signature either as we have so far or
-- with a one liner style like below!
valRead = read "25" :: Int


-- Functions with multiple arguments
makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number, street, town)

-- the function signature above certainly confused me when I first saw it. What
-- it shows is that makeAddress is a function that takes a Int arg and returns a
-- a function that itself takes a String and returns a function that itself
-- takes a String and finally returns a tuple of Int, String, String.

-- The reason for this is that in Haskell, all functions behind the scenes take
-- a single argument. This enables the language feature of Partial Application
-- Another way to implement makeAddress would be like below:
makeAddressLambda :: Int -> String -> String -> (Int, String, String)
makeAddressLambda = (\number ->
                      (\street ->
                        (\town -> (number, street, town))))

address :: (Int, String, String)
address = makeAddressLambda 27 "String Road" "Hull"

-- which itself is syntactic sugar for
address2 = (((makeAddressLambda 27) "String Road") "Hull")

-- note that in the function definition, the parentheses group to the right
-- while in the calling code they group to the left

-- First-class functions
ifEven :: (Int -> Int) -> Int -> Int
ifEven f n = if even n
             then f n
             else n

-- Type Variables
-- type variables allow for polymorphic functions
simple :: a -> a
simple x = x


-- Questions
-- 1. What is the type signature of filter?
-- filter :: (a -> a) -> [a] -> [a]
-- its different from map as it must return the same type of list
-- map :: (a -> b) -> [a] -> [b]

-- 2. Write a version of head that returns an empty list instead of error when
-- given an empty list
head2 :: [a] -> [a]
head2 []     = []
head2 (x:xs) = [x]

-- 3. Define the type signature of the myFoldl function from lesson 9
myFoldl :: (a -> a -> a) -> a -> [a] -> a
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
                          where newInit = f init x

foldResult = myFoldl (+) 0 [1,2,3,4,5]

main :: IO()
main = print valFromString
