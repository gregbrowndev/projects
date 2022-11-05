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
double n :: Int -> Int
double n = n * 2

-- Casting
-- We cannot divide a whole number in half, instead we must convert it to a Double
-- Note: Haskell has no concept of implicit casting, instead we must use functions to convert from one type to another
half :: Int -> Double
half n = (fromIntegral n) / 2
-- Haskell is stricter and at the same type more flexible than Python or Ruby. It is stricter by prohibiting implicit
-- casting, while it is more flexible as literal numbers are treated polymorphically - their type is inferred from the
-- context by the compiler. For this reason, we don't need to convert the divisor '2' to a Double.



main :: IO()
main = print check