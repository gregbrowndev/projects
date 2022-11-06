import Data.List

-- In this lesson, we'll look at how to implement existing type classes.
-- Overview:
--  - Implement your own type classes
--  - Understand polymorphism in Haskell
--  - Know when to use deriving
--  - Search for documentation with Hackage and Hoogle


-- Implementing Show
-- lets say we have a type to represent a 6 sided die:
data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6

-- while we could use deriving (Show) to support printing the values as strings,
-- we instead want something more meaningful, e.g. "one", "two", etc. To do this
-- we need to implement the Show type class ourselves.
instance Show SixSidedDie where
  show S1 = "one"
  show S2 = "two"
  show S3 = "three"
  show S4 = "four"
  show S5 = "five"
  show S6 = "six"

-- Great, that's it!
-- ghci> S1
-- one
-- ghci> S5
-- five

-- It might seem possible to avoid all this and just define a function:
-- show :: SixSidedDie -> String
-- show S1 = "one"
-- show S2 = "two"
-- etc. This code compiles. However, in GHCi, there are two issues. Firstly,
-- GHCi can no longer print the data constructors by default, instead you have
-- to use `show S1`. Secondly, manually calling `show S1` throws an error that
-- complains that `show` is ambiguous.

-- The function show above has already been defined. We cannot just redefine it
-- for SixSidedDie. In previous lessons, we've created functions like showName
-- to avoid the name collision.

-- What we really want is for the function show to behave polymorphically, i.e.
-- behave differently depending on the type of data.


-- Default Implementation and Minimum Complete Definition
-- we want to now make SixSidedDie comparable with Eq and Ord. Note that Eq is
-- a superclass of Ord. To implement these type classes, we would use deriving
-- as the compiler can figure it out for us. But let's do it explicitly:
instance Eq SixSidedDie where
  (==) S6 S6 = True
  (==) S5 S5 = True
  (==) S4 S4 = True
  (==) S3 S3 = True
  (==) S2 S2 = True
  (==) S1 S1 = True
  (==) _ _   = False

-- Note: we don't need to implement the (/=) operator even though that is part
-- of Eq. This is because (\=) has a default implementation of simply not (==).

-- To understand what functions in a type class are required, we can check the
-- docs on Hackage. Additionally, :info shows the "Minimum complete definition":

  -- ghci> :info Eq
  --  ...
  --  {-# MINIMAL (==) | (/=) #-}

-- This shows either (==) or (/=) must be defined. Let's look at Ord:

  -- ghci> :i Ord
  -- ..
  -- compare :: a -> a -> Ordering
  -- (<=) :: a -> a -> Bool
  -- {-# MINIMAL compare | (<=) #-}

-- This shows the (<=) or the compare functions must be defined.
-- The compare function takes two values and returns an Ordering. Ordering is
-- defined as `data Ordering = LT | EQ | GT`

-- To implement Ord's compare function ourselves is quite tidious:
instance Ord SixSidedDie where
  compare S6 S6 = EQ
  compare S6 _  = GT
  compare _ S6  = LT
  compare S5 S5 = EQ
  compare S5 _  = GT
  compare _ S5  = LT
  -- and so on. This would be painful for a 20-sided die!

-- The default behaviour of deriving (Ord) defines the order by the order in
-- which the data constructors are defined.

-- Better still is to use deriving (Enum) which represents the data constructors
-- as an enumerated list of constants. This is exactly what the sides of a die
-- are.

  -- ghci> :i Enum
  -- type Enum :: * -> Constraint
  -- class Enum a where
  --   toEnum :: Int -> a
  --   fromEnum :: a -> Int
  --   ...
  --   {-# MINIMAL toEnum, fromEnum #-}

-- Let's implement Enum for SixSidedDie
instance Enum SixSidedDie where
  toEnum 0 = S1
  toEnum 1 = S2
  toEnum 2 = S3
  toEnum 3 = S4
  toEnum 4 = S5
  toEnum 5 = S6
  toEnum _ = error "No such value"
  fromEnum S1 = 0
  fromEnum S2 = 1
  fromEnum S3 = 2
  fromEnum S4 = 3
  fromEnum S5 = 4
  fromEnum S6 = 5

-- Enum provides a number of benefits. Due to succ and pred, we can generate
-- lists of values:

  -- ghci> [S1 .. S6]
  -- [one,two,three,four,five,six]

-- However, if we generate a infinite list we get a error!

  -- ghci> [S1 ..]
  -- [one,two,three,four,five,six,*** Exception: No such value

-- If we allowed Haskell to derive the Enum type class, this wouldn't be a
-- problem. E.g.
-- data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Show, Enum)

-- Implementing a custom Ord
-- In lesson 4, we used a first-class function to order a tuple of names
type Name = (String, String)

names :: [Name]
names = [ ("Eugene", "Thacker"), ("Emil", "Cioran"), ("Friedrich", "Nietzsche")]

-- note we imported Data.List at the top of the script
sortedNames = sort names

  -- ghci> sortedNames
  -- [("Emil","Cioran"),("Eugene","Thacker"),("Friedrich","Nietzsche")]

-- As this shows, the type synonym that is just a pair of Strings is orderable
-- However, we'd like them to be ordered by last name then first name. We could
-- try to implement Ord for Name. However, we'd get an error, since the compiler
-- already knows how to order a pair of Strings. To fix this, we need to create
-- a new data type.

data Fullname = Fullname (String, String) deriving (Show, Eq)
instance Ord Fullname where
  compare (Fullname (f1, l1)) (Fullname (f2, l2)) = compare (l1, f1) (l2, f2)

fullNames :: [Fullname]
fullNames = [ Fullname ("Eugene", "Thacker")
            , Fullname ("Emil", "Cioran")
            , Fullname ("Friedrich", "Nietzsche")]

sortedFullNames = sort fullNames

  -- ghci> sortedFullNames
  -- [Fullname ("Emil","Cioran"),Fullname ("Friedrich","Nietzsche"),Fullname ("Eugene","Thacker")]

-- Creating types with newtype
-- Haskell provides an additional, more efficient way to define types such as
-- Name above. That is using the newtype keyword. Any type created with newtype
-- can also be created using data. However, the reverse isn't true, since types
-- created with newtype can only have one data constructor. The other benefit
-- of newtype is it allows for creation of a full type with its own type classes
-- rather than just a synonym of the underlying types.
newtype NameFancy = NameFancy (String, String) deriving (Show, Eq)

-- Type class roadmap
-- Section 14.8 shows a nice diagram of the type classes and the superclass
-- relationships between them.
-- In unit 2 (this unit), we've seen Enum, Eq, Ord, Show, Read, Bounded, and
-- Num. In unit 3, we'll be introduced to the abstract type classes Semigroup
-- and Monoid where we'll start to see how different type classes are to
-- interfaces. In unit 5, we'll look at a family of type classes, Functor,
-- Applicative, and Monad, that model the context of a computation. This last
-- group will be challenging to learn, but provide some of Haskell's most
-- powerful abstractions.

-- Questions
-- Q14.1: Create a type that derives Enum and use it to manually define the Eq
-- and Ord types. You should see how much easier it makes defining these classes
data Icecream = Chocolate | Strawberry deriving (Enum)
-- Hint: Enum maps each data constructor to an Int which implement Eq and Ord
instance Eq Icecream where
  -- don't write this:
--  (==) Chocolate Chocolate = True
--  (==) Strawberry Strawberry = True
--  (==) _ _ = False
  -- instead:
  (==) i1 i2 = (fromEnum i1) == (fromEnum i2)

instance Ord Icecream where
  compare i1 i2 = compare (fromEnum i1) (fromEnum i2)

-- Q14.2: Define a 5-sided die, FiveSidedDie, and a type class, Die, with at
-- least one useful function for a die. Make FiveSidedDie an instance of Die.
data FiveSidedDie = Sf1 | Sf2 | Sf3 | Sf4 | Sf5 deriving (Show, Enum, Eq, Ord, Bounded) -- avoid name collision
class Bounded a => Die a where
  cheat :: a -> a

instance Die FiveSidedDie where
  cheat _ = Sf5  -- cheat and roll a 5


-- Note: I wanted to write a Die function, roll, that returns a random value.
-- This can be done by requiring the Die is a subclass of Bounded since we can
-- use the minBound and maxBound to specify the range of ints, e.g.
-- randomR (minBound, maxBound). However, generating random numbers is a bit
-- involved since it requires IO and we haven't covered that yet! Instead, I
-- wrote the function, cheat, which returns the best roll.

-- The solution basically was doing the same thing, but they make it
-- deterministic
class (Eq a, Enum a) => Die2 a where
  roll :: Int -> a


instance Die2 FiveSidedDie where
  roll n = toEnum (n `mod` 5)

  -- ghci> roll 7 :: FiveSidedDie
  -- Sf3

-- Note: we could still use fromEnum(maxBound) in place of the 5 to make this
-- generic for all Die (however, I'm not sure how to add default implement yet)