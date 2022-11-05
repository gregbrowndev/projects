import           Data.Char (toLower)

-- The functions below follow the same pattern of
-- applying some operation element-wise to a list
add3ToAll []       = []
add3ToAll (x : xs) = (x + 3) : add3ToAll xs

mul3ToAll []       = []
mul3ToAll (x : xs) = (x * 3) : mul3ToAll xs

-- we can abstract out the operation to create a
-- higher-order function
applyToAll _ []          = []
applyToAll func (x : xs) = (func x) : applyToAll func xs

-- 9.2 Understanding how map works
-- try to implement the two functions below without using map
-- map ("a " ++) ["train", "plane", "boat"]
addAToAll []       = []
addAToAll (x : xs) = ("a " ++ x) : addAToAll xs

-- map (^2) [1,2,3]
squareAll []       = []
squareAll (x : xs) = (x ^ 2) : squareAll xs

-- filter
-- filter even [1,2,3]
myFilter _ [] = []
myFilter pred (x : xs) = if pred x then x : rest else rest
  where
    rest = myFilter pred xs

-- remove
remove _ [] = []
remove pred (x : xs) = if pred x then rest else x : rest
  where
    rest = remove pred xs

-- Using foldl
-- Write a function to calculate the product
-- foldl (*) 1 [1,2,3,4]
myProduct []       = 1
myProduct (x : xs) = x * myProduct xs

-- Using map and foldl to create sumOfSquares
sumOfSquares xs = foldl (+) 0 squares
  where
    squares = map (^ 2) xs

-- reversing a list. Note, we need a helper function to reverse cons
rcons x y = y : x

myReverse xs = foldl rcons [] xs

-- implement foldl yourself
myFoldl _ init []          = init
myFoldl func init (x : xs) = func x (myFoldl func init xs)

myFoldr _ init [] = init
myFoldr func init (x : xs) = func x rightResult
  where
    rightResult = myFoldr func init xs

-- Questions
-- Use the filter and length to re-create the elem function
myElem2 _ []         = False
myElem2 val (x : xs) = (val == x) || myElem2 val xs

myElem val xs = length filtered /= 0
  where
    filtered = filter (== val) xs

-- Write a isPalindrome function
isPalindrome :: [Char] -> Bool
isPalindrome xs = cleaned == reverse cleaned
  where
    cleaned = map toLower (filter (== ' ') xs)

-- Harmonic series
harmonic2 0 = 0
harmonic2 n = harmonic2 (n -1) + 1 / n

harmonic n = sum (take n series)
  where
    series = map (1 /) [1 ..]


harmonic3 n = sum (take n seriesValues)
  where
    seriesPairs = zip (repeat 1.0) [1.0, 2.0 ..]
    seriesValues = map (uncurry (/)) seriesPairs
