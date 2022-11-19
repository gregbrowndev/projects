import           Data.List
import qualified Data.Map  as Map

{-
Q18.1: For the types Triple and Box, impl a function similar to map: tripleMap
       and boxMap.
-}
data Triple a = Triple a a a
  deriving (Show, Eq)

first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ y _) = y

third :: Triple a -> a
third (Triple _ _ z) = z

-- Note: my solution went down the wrong path. I was assuming tripleMap should
-- work with a list of Triples, but really map works with a container
-- so in this case Triple is the container.

--tripleMap :: (a -> b) -> [Triple a] -> [Triple b]
--tripleMap f ts = map fTriple ts
--  where fTriple = (\t -> Triple (f (first t)) (f (second t)) (f (third t)))

-- The solution is simpler

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap f (Triple x y z) = Triple (f x) (f y) (f z)


type Point3D = Triple Double
points :: [Point3D]
points = [Triple 0.1 53.2 12.3, Triple 3.2 94.2 1.3, Triple 0.0 1.8 10.3]

newPoints :: [Triple String]
--newPoints = tripleMap (show) points    -- from my initial solution
newPoints = map (\t -> tripleMap (show) t) points

{-
  ghci> newPoints
  [Triple "0.1" "53.2" "12.3",Triple "3.2" "94.2" "1.3",Triple "0.0" "1.8" "10.3"]
-}

data Box a = Box a deriving (Show)

boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box x) = Box (f x)

boxes :: [Box Int]
boxes = [Box 1, Box 2, Box 3, Box 4]

newBoxes :: [Box String]
newBoxes = map (\b -> boxMap (show) b) boxes

{-
  ghci> newBoxes
  [Box "1",Box "2",Box "3",Box "4"]
-}

{-
Q18.2: Modify Organ type so it can be used as a key. Build a map,
       organInventory, that maps each organ in organCatalog to its count.
-}

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord, Enum)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

countValues :: Ord a => [a] -> [(a, Int)]
countValues xs = map (\w -> (head w, length w)) $ group $ sort xs

{-
The dollar signs avoid parentheses giving precedence to the right, e.g. avoids

  countValues xs = map (\w -> (head w, length w)) (group (sort xs))

Useful functions used in impl. Note: sort comes from Data.List

  ghci> :t sort
  sort :: Ord a => [a] -> [a]
  ghci> :t group
  group :: Eq a => [a] -> [[a]]
-}

organInventory :: Map.Map Organ Int
organInventory = Map.fromList (countValues organs)

{-
  ghci> organInventory
  fromList [(Heart,2),(Brain,1),(Kidney,1),(Spleen,2)]
-}
