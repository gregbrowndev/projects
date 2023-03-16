{- Lesson 29: Lists as Context - A deeper look at the Applicative type class

After this lesson, we'll be able to:

    - Explain the formal definition of the Applicative type class
    - Represent parameterised types as either containers or contexts
    - Use List as a context to explore nondeterministic computing

Consider this: a breakfast place offers the choice of the following:

    - Coffee or tea
    - Eggs, pancakes, or waffles
    - Toast or a biscuit
    - Sausage, ham, or bacon

What are all the possible meals you could choose, and how can you use List to help?

Because of the way Applicative works with Functor, Functor is a superclass of
Applicative:

    Functor
    fmap  :: Functor f :: (a -> b) -> f a -> f b
    (<$>) :: Functor f :: (a -> b) -> f a -> f b

    (superclass of)

    Applicative
    (<*>) :: Applicative f :: f (a -> b) -> f a -> f b
    pure  :: Applicative f :: a -> f a

Note: unlike <$> which has an analogous function fmap, the <*> "app" infix operator
has no prefix form.
-}

{- 29.1.1: The pure method

The function pure is the second required by the Applicative type class. The pure
method is a useful helper function for taking an ordinary value or function and
putting it into a context. The best way to learn it is to play around with it in
GHCi:

    ghci> pure 6 :: Maybe Int
    Just 6

    ghci> pure "Hello World" :: IO String
    "Hello World"

We can also use it to put functions into the context of an Applicative:

    ghci> (6+) <$> Just 5
    Just 11

    ghci> pure (6+) <*> Just 5
    Just 11
-}

{- 29.2 Containers vs. contexts

So far, we'll loosely talked about the difference between parameterised types
that represent containers and contexts. We need to be more precise, because unlike
Functor and Applicative, the next lesson's type class Monad only makes sense when
we talk about contexts. Here's the distinction in a nutshell:

    - Parameterised types that represent a container are types that represent a
      data structure
    - When a type is a context, extra information is implied about the type beyond
      its structure

Let's look at an example, List. It's easy to understand that List is a container, but
it also describes a context. If we understand why, we'll be on the path to truly
understand Applicatives. List is a member of Applicative, so there must be a way
to view List as a context. The reason context matters is because we can ask the
question, "What does it mean to apply a function to two or more values in the
context of a list?" For example, what does [1,2,3] + [4,5] mean? The naive
assumption would be: [1,2,3,4,5]. This is just concatenating the two lists
(the ++ operator for lists).

We want to combine two values in the context of a List using addition in a
cartesian fashion. We can write this using Applicative:

    ghci> pure (+) <*> [1,2,3] <*> [4,5]
    [5,6,6,7,7,8]

As we can see, adding two ints in the context of a list means adding all
possible combinations of those lists.

It is important to know that List is both a container and a context:

    - A list as a container is a sequence of values that can hold any type.
      Each item in the list points to the next one or to the empty list.
    - A list as a context represents a set of possibilities. This of a list
      as a context as being a single variable that can container many possible
      values.

Maybe and IO are simple contexts to understand. An [Int] is an Int in the context
that there are many possible values for that Int. Because there are many possible
values for that [Int], when you apply a function (Int -> Int -> Int) in context of
a list, you must think non-deterministically and compute all possible results of
that operation.
-}

{- 29.3.2 A game show example

Let's say the contestant can pick from one of three doors worth different values
of cash, and behind these doors are two boxes worth two amounts of a cash. We can
easily compute all combinations to solve this problem non-deterministically.
-}
doorPrize :: [Int]
doorPrize = [1000, 2000, 3000]

boxPrize :: [Int]
boxPrize = [500, 20000]

totalPrize :: [Int]
totalPrize = pure (+) <*> doorPrize <*> boxPrize

-- Instead of thinking about [Int] as a container of ints (deterministic), think
-- of it as an Int that can take on a range of values (non-deterministic).

prizeMultiplier :: [Int]
prizeMultiplier = [10, 50]

totalPrize2 :: [Int]
totalPrize2 = pure (*) <*> doorPrize <*> prizeMultiplier

{- 29.3.3 Generating N prime numbers

We can use the Applicative properties of a list to compute the first N prime
numbers in an amazingly simple way:

    - Start with your list from 2 to n
    - Determine all the non-prime numbers (composite numbers)
    - Filter out all items from the list that aren't composite

We can determine the list of composite numbers by multiplying the list
from 2 to n with itself.
-}
twoThroughN :: [Int]
twoThroughN = [2 .. 10]

compositeNumbers :: [Int]
compositeNumbers = pure (*) <*> twoThroughN <*> twoThroughN

primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite twoThroughN
    where twoThroughN = [2 .. n]
          composite = pure (*) <*> twoThroughN <*> twoThroughN
          isNotComposite = not . (`elem` composite)

{-
    ghci> primesToN 32
    [2,3,5,7,11,13,17,19,23,29,31]
-}

{- 29.3.4 Generating large amounts of test data

-}

data User = User {
    name    :: String,
    gamerId :: Int,
    score   :: Int
} deriving Show

testNames :: [String]
testNames = ["John Smith"
 ,"Robert'); DROP TABLE Students;--"
 ,"Christina NULL"
 ,"Randall Munroe"]

testIds :: [Int]
testIds = [1337
 ,0123
 ,999999]

testScores :: [Int]
testScores = [0
 ,100000
 ,-99999]

testUsers :: [User]
testUsers = pure User <*> testNames <*> testIds <*> testScores

{-
    ghci> take 3 testUsers
    [User {name = "John Smith", gamerId = 1337, score = 0},
     User {name = "John Smith", gamerId = 1337, score = 100000},
     User {name = "John Smith", gamerId = 1337, score = -99999}]
-}

{- Summary

Our objective this lesson was to gain a deeper insight into the Applicative type
class by thinking about lists as a context. Contexts differ from containers in that
they require you to understand something about the computation beyond what the
data structure tells you alone. For lists, this means representing non-deterministic
computation, rather than just a computation for sequential data.
-}

{- Q29.1
To prove that Applicative is strictly more powerful than Functor, write a universal
version of fmap, called allFmap, that defines fmap for all members of the Applicative type
class. Because it works for all instances of Applicative, the only functions you can use are
the methods required by the Applicative type class. To get you started, here’s your type
signature:

    allFmap :: Applicative f => (a -> b) -> f a -> f b
-}

allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap f = (pure f <*>)

{-
    ghci> allFmap (+1) [1,2,3]
    [2,3,4]
    ghci> allFmap (+1) (Just 5)
    Just 6
    ghci> allFmap (+1) Nothing
    Nothing
-}

{- Q29.2
Translate the following expression into one where the result is a Maybe Int. The
catch is that you may not add (or remove) anything to the code except pure and <*>.
You can’t use the Just constructor or any extra parentheses.

    example :: Int
    example = (*) ((+) 2 4) 6
-}

example :: Maybe Int
example = pure (*) <*> (pure (+) <*> pure 2 <*> pure 4) <*> pure 6

-- It seems that the result of the expression is coerced into Maybe Int due
-- to the type signature, since typing this into GHCi just results in 6.

{- Q29.3
Take the following example and use nondeterministic computing with Lists to
determine how much beer you need to purchase to assure there will be enough:

    - You bought beer last night but don’t remember whether it was a 6-pack or
      a 12-pack.
    - You and your roommate each had two beers last night.
    - You’re having either two or three friends coming over tonight, depending
      on who can come.
    - For a long night of gaming, you expect the average person to drink three
      to four beers.
-}

