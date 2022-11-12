import           Data.List
import           Data.Semigroup

{- Lesson 17 - Design by Composition: Semigroups and Monoids

In this lesson, we will:
  - create new functions with function composition
  - use Semigroup to mix colours
  - learn how to use guards in code
  - solve probability problems with Monoid

We saw in the last lesson that sum types allow you to think outside of the
traditional hierarchical design patterns. Another important part of Haskell that
differs from most popular language is the deep rooted principle of
composability. We can combine many things, e.g. two lists to get a new list,
two colours to get a new colour, etc. In most languages each of these would have
its own function or operator, whereas in Haskell offers a standard way to
combine two things. It does this through the Semigroup type class.
-}

-- The compose operator
-- A special HOF called the compose operator composes two functions together:
myLast :: [a] -> a
myLast = head . reverse

myMin :: Ord a => [a] -> a
myMin = head . sort

myMax :: Ord a => [a] -> a
myMax = myLast . sort

myAll :: (a -> Bool) -> [a] -> Bool
myAll testFunc = (foldr (&&) True) . (map testFunc)

myAny :: (a -> Bool) -> [a] -> Bool
myAny testFunc = (foldr (||) False) . (map testFunc)

{-
  ghci> myAll (\x -> x >= 10) [15, 25, 63]
  True
  ghci> myAll (\x -> x >= 10) [15, 25, 63, 8]
  False
  ghci> myAny (\x -> x == 6) [2,4,6]
  True
  ghci> myAny (\x -> x == 6) [2,4,5]
  False
-}

-- The Semigroup type class
-- A simple example is to define Integer as an instance of the Semigroup class.
-- The <> operator is the combine function
instance Semigroup Integer where
  (<>) x y = x + y

-- The type signature for <> is:
-- (<>) :: Semigroup a => a -> a -> a

-- The Colour Semigroup
data Colour = Red
  | Yellow
  | Blue
  | Green
  | Purple
  | Orange
  | Brown deriving (Show, Eq, Enum)

instance Semigroup Colour where
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Orange
-- This case is not associative, we'll add guards to adhere to the law
--  (<>) a b = if a == b
--             then a
--             else Brown
  (<>) a b | a == b                                   = a
           | all (`elem` [Red,Blue,Purple]) [a,b]     = Purple
           | all (`elem` [Blue,Yellow,Green]) [a,b]   = Green
           | all (`elem` [Red, Yellow,Orange]) [a,b]  = Orange
           | otherwise                                = Brown
  -- note, this case could replace all the other cases

{-
  ghci> Yellow <> Blue
  Green
  ghci> Yellow <> Red
  Orange
  ghci> Red <> Red
  Red
-}

-- However, one major problem with our Colour Semigroup is that it is not
-- associative. This is mandatory to be formally a Semigroup.

{-
  ghci> (Green <> Blue) <> Yellow
  Brown
  ghci> Green <> (Blue <> Yellow)
  Green
-}


-- Many type classes have type class laws that require certain behaviour. These
-- are tests that are written in terms of the type class that must hold for any
-- instance that was to implement the class. However, the Haskell compiler can't
-- enforce these itself.

-- Making Colour associative using guards
-- Guards work like pattern matching but they allow some computation on the args
-- you're going to PM.
howMuch :: Int -> String
howMuch n | n > 10 = "a whole bunch"
          | n > 0 = "not much"
          | otherwise = "we're in debt!"

-- We'll add guards to the Colour Semigroup
-- It now works correctly

{-
  ghci> (Green <> Blue) <> Yellow
  Green
  ghci>
  ghci> Green <> (Blue <> Yellow)
  Green
-}

{-
Composing with identity: Monoids
A Monoid extends Semigroup but adds an identity property:
x <> id = x (and id <> x = x)
For ints, the identity is 0. The Colour type doesn't yet have an identity.
Adding an identity to a type greatly increases its utility as we can use
fold functions to easily combine lists of the same type.

Note: one would expect Monoid to be defined as a subclass of Semigroup. However,
Monoid predates Semigroup in the Haskell type system. Due to this it has an
unfortunate set of signatures:

  ghci> :i Monoid
  type Monoid :: * -> Constraint
  class Semigroup a => Monoid a where
    mempty :: a
    mappend :: a -> a -> a
    mconcat :: [a] -> a

The functions "m" functions historically come from the fact that List is the
most common Monoid, so these are just the names of List's identity and combine
functions suffixed with 'm' for Monoid.

In the Monoid class, we just need to impl mempty and mappend and we get mconcat
for free (remember minimally complete definition from unit 2).

  ghci> mconcat ["does", "this", "make", "sense?"]
  "doesthismakesense?"

The definition of mconcat is just:

  mconcat = foldr mappend mempty

The impl uses foldr instead of foldl because foldr can work with infinite lists,
whereas foldl will force the evaluation.
-}

{- Monoid Laws

There are four Monoid laws:
  1. `mappend mempty x == x`, e.g. `[] ++ [1, 2, 3] == [1, 2, 3]`
  2. `mappend x mempty == x`, i.e. the first law reversed
  3. `mappend x (mappend y z) == mappend (mappend x y) z`, i.e. associativity
  4. `mconcat = foldr mappend mempty`, i.e. the definition of mconcat
-}

-- Practical: building probability tables
-- Let's look at a simple example, working with flipping a coin
type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs

-- a PTable factory function that ensures the probabilities sum to 1. This
-- can be done by dividing each probability by the sum of all probabilities
createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalisedProbs
  where normalisedProbs = map (\p -> p / totalProbs) probs
        totalProbs = sum probs

-- Let's make PTable showable with nice formatting
showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
  show (PTable events probs) = mconcat pairs
    where pairs = zipWith (showPair) events probs

ptTable :: PTable
ptTable = createPTable ["heads", "tails"] [0.5, 0.5]

{-
  ghci> ptTable
  heads|0.5
  tails|0.5
-}


{-
We want to be able to model the probability table of two coins, e.g.

  heads-heads|0.25
  heads-tails|0.25
  tails-heads|0.25
  tails-tails|0.25

We need to compute the cartesian product of all events and probabilities. We'll
write a generic function that takes a function for combining two lists, two
lists, and returns a new list.
-}

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func l1' l2'
  where nToAdd = length l2
        -- repeat each element in l1 for every element in l2
        l1Repeated = map (take nToAdd . repeat) l1
        -- l1Repeated is a list of lists, so concat
        l1' = mconcat l1Repeated
        -- loop through l2 as many times as needed
        l2' = cycle l2

-- we can reuse this function to combine events and probabilities
combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartCombine combiner e1 e2
  where combiner = (\x y -> mconcat [x, "-", y])

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = cartCombine (*) p1 p2

-- Now we can make PTable an instance of Semigroup and Monoid
instance Semigroup PTable where
  (<>) ptable1 (PTable [] []) = ptable1
  (<>) (PTable [] []) ptable2 = ptable2
  (<>) (PTable e1 p1) (PTable e2 p2) = PTable e3 p3
    where e3 = combineEvents e1 e2
          p3 = combineProbs p1 p2

instance Monoid PTable where
  mempty = PTable [] []
  mappend = (<>)

-- Let's test our monoid PTable for fair coins and biased spinner
coin :: PTable
coin = createPTable ["heads", "tails"] [0.5, 0.5]

spinner :: PTable
spinner = createPTable ["red", "blue", "green"] [0.1, 0.2, 0.7]

{-
With this we can compute the odds for each combination!

  ghci> coin <> spinner
  heads-red|5.0e-2
  heads-blue|0.1
  heads-green|0.35
  tails-red|5.0e-2
  tails-blue|0.1
  tails-green|0.35

From this we see there is 0.1 (10%) chance to land tails and blue

What's the probability of landing head three times in a row?

  ghci> coin <> coin <> coin
  heads-heads-heads|0.125
  ...

The answer is 12.5%.
-}
