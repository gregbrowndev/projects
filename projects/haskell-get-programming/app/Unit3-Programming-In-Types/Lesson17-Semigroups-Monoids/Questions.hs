-- Questions
-- Q17.1: Make Colour an instance of Monoid
data Colour = Red
  | Yellow
  | Blue
  | Green
  | Purple
  | Orange
  | Brown deriving (Show, Eq, Enum)

instance Semigroup Colour where
  (<>) a b | a == b                                   = a
           | all (`elem` [Red,Blue,Purple]) [a,b]     = Purple
           | all (`elem` [Blue,Yellow,Green]) [a,b]   = Green
           | all (`elem` [Red, Yellow,Orange]) [a,b]  = Orange
           | otherwise                                = Brown

instance Monoid Colour where
  mempty = Blue
  mappend = (<>)


-- Q17.2: If Events and Probs were data types rather than just synonyms, they
--        could also be instances of Semigroup and Monoid. Refactor the code
--        above.

-- keep the helper function as before
cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func l1' l2'
  where nToAdd = length l2
        -- repeat each element in l1 for every element in l2
        l1Repeated = map (take nToAdd . repeat) l1
        -- l1Repeated is a list of lists, so concat
        l1' = mconcat l1Repeated
        -- loop through l2 as many times as needed
        l2' = cycle l2

-- Refactor Events into a data type that is an instance of Semigroup and Monoid
-- (note, we don't need the combineEvents function anymore)
data Events = Events [String]

instance Semigroup Events where
  (<>) (Events e1) (Events e2) = Events (cartCombine combiner e1 e2)
     where combiner = (\x y -> mconcat [x, "-", y])

instance Monoid Events where
  mempty = Events []
  mappend = (<>)


-- Refactor Probs into data type...
data Probs = Probs [Double]

instance Semigroup Probs where
  (<>) (Probs p1) (Probs p2) = Probs (cartCombine (*) p1 p2)

instance Monoid Probs where
  mempty = Probs []
  mappend = (<>)

data PTable = PTable Events Probs

-- a PTable factory function that ensures the probabilities sum to 1. This
-- can be done by dividing each probability by the sum of all probabilities
createPTable :: Events -> Probs -> PTable
createPTable (Events events) (Probs probs) = PTable (Events events) (Probs normalisedProbs)
  where normalisedProbs = map (\p -> p / totalProbs) probs
        totalProbs = sum probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
  show (PTable (Events events) (Probs probs)) = mconcat pairs
    where pairs = zipWith (showPair) events probs

-- We can refactor Semigroup PTable as we no longer need the special cases since
-- empty cases are handled in the Events and Probs
instance Semigroup PTable where
  (<>) (PTable e1 p1) (PTable e2 p2) = PTable e3 p3
    where e3 = e1 <> e2
          p3 = p1 <> p2

instance Monoid PTable where
  mempty = PTable mempty mempty
  mappend = (<>)


-- Let's test it still works
coin :: PTable
coin = createPTable (Events ["heads", "tails"]) (Probs [0.5, 0.5])

spinner :: PTable
spinner = createPTable (Events ["red", "blue", "green"]) (Probs [0.1, 0.2, 0.7])

{-
  ghci> coin <> spinner
  heads-red|5.0e-2
  heads-blue|0.1
  heads-green|0.35
  tails-red|5.0e-2
  tails-blue|0.1
  tails-green|0.35

  ghci> coin <> coin <> coin
  heads-heads-heads|0.125

Looking good!
-}
