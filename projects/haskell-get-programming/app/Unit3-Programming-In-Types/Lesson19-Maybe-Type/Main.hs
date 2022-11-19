import           Data.List
import qualified Data.Map   as Map
import           Data.Maybe

{- Lesson 19 - The Maybe Type

After this lesson, I'll be able to:

  - understand the Maybe type
  - Use the Maybe type to handle missing values
  - Design programs with Maybe types

Just as type classes can often be much more abstract than interfaces in OOP,
parameterised types play a much larger role than generics do in most languages.

Unlike List or Map, which are containers for values, Maybe is the first of many
types we'll see that represent a context for a value. Maybe types represent
values that might be missing.

Maybe is defined as:

  data Maybe a = Nothing | Just a
-}

wordCount :: Map.Map String Int
wordCount = Map.fromList [("Red", 1), ("Green", 4), ("Blue", 9)]

{-
As we can see, we get back the data constructors Just or Nothing when we use
the Map.lookup function

  ghci> Map.lookup "Green" wordCount
  Just 4
  ghci> Map.lookup "green" wordCount
  Nothing
-}

{-
Let's look at a practice example using Maybe. Continuing from our mad scientist
example from Lesson 18, we want to count the number of a given organ in all
drawers
-}

-- As before...
data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord, Enum)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]
ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

makeOrganCatalog :: [Int] -> [Organ] -> Map.Map Int Organ
makeOrganCatalog drawIds organs = Map.fromList organPairs
  where organPairs = zip drawIds organs

organCatalog :: Map.Map Int Organ
organCatalog = makeOrganCatalog ids organs

-- List of possible drawer ids
possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

-- A function to get the contents of each drawer
getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents drawerIds catalog = map getContents drawerIds
  where getContents = \id -> Map.lookup id catalog

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

-- A function to the count of an organ we are interested in:
countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = length justOrgans
  where justOrgans = filter (\m -> m == Just organ) available

{-
The interesting thing here is that Maybe implements Eq, so you don't even need
to unpack it, we can just compare `x == Just organ`.

  ghci> countOrgan Heart availableOrgans
  2
  ghci> countOrgan Kidney availableOrgans
  1

We can filter and pattern matching to get a list of just the organs:
-}

isSomething :: Maybe Organ -> Bool
isSomething Nothing  = False
isSomething (Just _) = True

justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isSomething availableOrgans

{-
  ghci> justTheOrgans
  [Just Heart,Just Heart,Just Brain,Just Spleen,Just Spleen,Just Kidney]
-}

-- Note, the Data.Maybe module provides isJust and isNothing functions, we don't
-- have to write them ourselves.

showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing      = ""

{-
  ghci> showOrgan (Just Heart)
  "Heart"
  ghci> showOrgan Nothing
  ""
-}

organList :: [String]
organList = map showOrgan justTheOrgans

{-
  ghci> organList
  ["Heart","Heart","Brain","Spleen","Spleen","Kidney"]
-}

cleanList :: String
cleanList = intercalate ", " organList

{-
  ghci> cleanList
  "Heart, Heart, Brain, Spleen, Spleen, Kidney"
-}

-- Quick check 19.2:
numOrZero :: Maybe Int -> Int
numOrZero (Just n) = n
numOrZero Nothing  = 0

{-
  ghci> numOrZero (Just 12)
  12
  ghci> numOrZero Nothing
  0
-}

-- More complex example: 19.4
data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
  show (Vat organ)    = show organ ++ " in a vat"
  show (Cooler organ) = show organ ++ " in a cooler"
  show (Bag organ)    = show organ ++ " in a bag"

data Location = Lab | Kitchen | Bathroom deriving (Show)

storeOrgan :: Organ -> Container
storeOrgan Brain = Vat Brain
storeOrgan Heart = Cooler Heart
storeOrgan organ = Bag organ

storeContainer :: Container -> (Location, Container)
storeContainer (Vat a)    = (Lab, Vat a)
storeContainer (Cooler a) = (Lab, Cooler a)
storeContainer (Bag a)    = (Kitchen, Bag a)

process :: Organ -> (Location, Container)
process organ = (storeContainer . storeOrgan) organ

report :: (Location, Container) -> String
report (location, container) = show container ++
                               " in the " ++
                               show location

{-
  ghci> process Heart
  (Lab,Heart in a cooler)
  ghci> process Spleen
  (Kitchen,Spleen in a bag)
  ghci> process Brain
  (Lab,Brain in a vat)

  ghci> report (process Heart)
  "Heart in a cooler in the Lab"
  ghci> report (process Spleen)
  "Spleen in a bag in the Kitchen"

Now we want to put it all together to write a function processRequest that
looks for a given organ in our inventory and if found sends it to the location.
Now we need to handle Maybe!
-}

-- This code won't compile because organ is type Maybe Organ!
--processRequest ::  Int -> Map.Map Int Organ -> String
--processRequest id catalog = report (process organ)
--  where organ = Map.lookup id catalog

-- we can create a short term fix by writing a new func:
processAndReport :: (Maybe Organ) -> String
processAndReport (Just organ) = (report . process) organ
processAndReport Nothing      = "error, id not found"

processRequest ::  Int -> Map.Map Int Organ -> String
processRequest id catalog = processAndReport organ
  where organ = Map.lookup id catalog

{-
  ghci> processRequest 2 organCatalog
  "Heart in a cooler in the Lab"
  ghci> processRequest 66 organCatalog
  "error, id not found"

Our function works but its not ideal. We've had to add another function to
handle turning a Maybe Organ into the simple Organ argument that process
requires, and now we have error handling code in this new function. What if we
wanted to handle such an error in the report function? We could rewrite process
to accept a Maybe Organ. However, this would not be ideal as we would lose the
simplicity of knowing the value passed to process is definitely something!
-}

{- Questions

Q19.1: Write a function emptyDrawers
-}

emptyDrawers :: [Maybe Organ] -> Int
emptyDrawers contents = length empty
  where empty = filter isNothing contents

  {-
  ghci> emptyDrawers availableOrgans
  44
  -}

-- Q19.2: Write a version of map that works for Maybe types, called maybeMap.

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f (Just x) = Just (f x)
maybeMap f Nothing  = Nothing

{-
  ghci> maybeMap (show) (Just Heart)
  Just "Heart"
  ghci> maybeMap (show) Nothing
  Nothing
-}
