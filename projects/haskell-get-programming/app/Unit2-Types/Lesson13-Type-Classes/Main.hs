-- In this lesson, we'll learn about type classes and the common ones you find
-- in Haskell, such as Num, Show, Eq, Ord, and Bounded

-- A good way to learn about a type in GHCi is to use the :type or :t command.
-- E.g. ghci> :type (+)
--      (+) :: Num a => a -> a -> a

-- In the type signature above, we see "Num a =>". This is a type class! We can
-- learn more about it using the :info or :i command:
-- ghci> :info Num
-- type Num :: * -> Constraint
-- class Num a where
--   (+) :: a -> a -> a
--   (-) :: a -> a -> a
--   (*) :: a -> a -> a
--   negate :: a -> a
--   abs :: a -> a
--   signum :: a -> a
--   fromInteger :: Integer -> a

-- A type class describes a group of types that all behave the same way. Any
-- type that belongs to the Num type class must have the functions above.

-- With the Num type class, we can write functions that work on any Num types
addThenDouble :: Num a => a -> a -> a
addThenDouble x y = (x + y) * 2

r1 = addThenDouble 3 4      -- 14
r2 = addThenDouble 3.5 4.3  -- 15.6

-- Writing our own type class: Describable
class Describable a where
  describe :: a -> String

-- we could use this to make our types support a describe function that provides
-- a useful description of what it does

-- The Ord type class
-- Defines a group of types that are orderable. We can see if we look at the (>)
-- operator:
-- ghci> :t (>)
-- (>) :: Ord a => a -> a -> Bool

-- ghci> :i Ord
-- type Ord :: * -> Constraint
-- class Eq a => Ord a where
--   compare :: a -> a -> Ordering
--   (<) :: a -> a -> Bool
--   (<=) :: a -> a -> Bool
--   (>) :: a -> a -> Bool
--   (>=) :: a -> a -> Bool
--   max :: a -> a -> a
--   min :: a -> a -> a

-- In the definition of Ord, we see another type class Eq.
-- ghci> :i Eq
-- type Eq :: * -> Constraint
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool

-- It makes sense that for a type to be orderable, it must also support equality
-- However, the reverse is not always true, a type that supports equality may
-- not necessarily be orderable.

-- The Bounded type class
-- In lessson 11, we mentioned the difference between Int and Integer is that
-- the Int type is bound to a 64 bit number whereas Integer can represent any
-- whole number. This is because the Int type is an instance of the Bounded
-- type class.

-- ghci> :i Bounded
-- type Bounded :: * -> Constraint
-- class Bounded a where
--   minBound :: a
--   maxBound :: a

-- members of Bounded must provide a way to get the upper and lower bounds.
-- Note: theses are values, not functions!

-- ghci> minBound :: Int
--  -9223372036854775808
-- ghci> maxBound :: Int
--  9223372036854775807

-- The Show and Read type class
-- The Show and Read type classes allows types to support the show and read
-- functions. Let's look at an example

-- If we were to print out Chocolate in GHCi, we'd see:
-- data Icecream = Chocolate | Vanilla
-- ghci> Chocolate
--
-- <interactive>:73:1: error:
--    • No instance for (Show Icecream) arising from a use of ‘print’
--    • In a stmt of an interactive GHCi command: print it

-- Haskell can automatically derive the implementation of many type classes with
-- sensible behaviour, e.g.
data Icecream = Chocolate | Vanilla deriving (Show, Eq, Ord)

-- ghci> Chocolate
-- Chocolate
-- ghci> Chocolate == Vanilla
-- False
-- ghci> Vanilla > Chocolate
-- True

-- Questions
-- Q13.1: What is the Word type and how is it different from Int?
-- Inspecting these type shows they are belong to the same set of type classes
-- Using `minBound :: Word` shows a lower bound of 0, compared to Int's lower
-- bound of -9223372036854775808. This shows Word is an unsigned integer

-- Q13.2: Look at the Enum type class. Consider Int which is an instance of both
-- Enum and Bounded. Given the definition of inc (user defined function):
-- inc :: Int -> Int
-- int x = x + 1
-- What is the difference between inc and succ (required by Enum)?
-- Both of these functions return the value of the Int plus 1, e.g.
-- ghci> succ 1
-- 2
-- ghci> succ 2
-- 3

-- if you do `inc maxBound :: Int` you get back -9223372036854775808
-- while `succ maxBound :: Int` throws an error.
-- *** Exception: Prelude.Enum.succ{Int}: tried to take `succ' of maxBound

-- Q13.3: Write a function cycleSucc that cycles back around when it reaches
-- its bound. This function should work on a wide range of types, not just Num
-- types, i.e. if we have a type like Icecream above, we should be able to cycle
-- through the possible data constructors.
-- As a starting point, the definition below is given. There is a mystery type
-- class that you will need to use, and the impl will use functions from all
-- type classes

-- cycleSucc :: (Bounded a, Enum a, ? a) => a -> a
-- cycleSucc n = ?

-- Initially, I thought the mystery type class was Ord because it seemed that
-- you would need to know the ordering of the types to get the next. However,
-- Enum's succ function already returns the next value, so specifying Ord again
-- again is at most redundant. I then realised that the mystery type class has
-- to be Eq, because we need to check if n is equal to the upper bound.

cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound
                then minBound
              else succ n
