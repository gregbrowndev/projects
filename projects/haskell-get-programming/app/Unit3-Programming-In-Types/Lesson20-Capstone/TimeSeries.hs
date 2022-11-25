{- Lesson 20: Capstone Project - Time Series

This capstone covers:
  - learning the basics of time-series analysis
  - combining multiple time series with Monoid and Semigroup
  - using Map to solve problems of duplicate values in a time series
  - avoiding errors involving missing values by using Maybe

We'll expore how to combine multiple times series into one, take summary
statistics (such as the average) of time-series data  with missing values, and
conclude by performing transformations on the data such as smoothing to
eliminate noise.

Suppose we work for a financial company and we've been asked to organise its
financial data. We have 36 months (partial) of data in 4 separate files. No file
has a complete set of data.

Note: we haven't covered how to read files yet in Haskell, so we'll assume the
data is already loaded.
-}
import           Data.List
import qualified Data.Map       as Map
import           Data.Maybe
import           Data.Semigroup


file1 :: [(Int, Double)]
file1 =
  [ (1 , 200.1)
  , (2 , 199.5)
  , (3 , 199.4)
  , (4 , 198.9)
  , (5 , 199.0)
  , (6 , 200.2)
  , (9 , 200.3)
  , (10, 201.2)
  , (12, 202.9)
  ]

file2 :: [(Int, Double)]
file2 =
  [ (11, 201.6)
  , (12, 201.5)
  , (13, 201.5)
  , (14, 203.5)
  , (15, 204.9)
  , (16, 207.1)
  , (18, 210.5)
  , (20, 208.8)
  ]

file3 :: [(Int, Double)]
file3 =
  [ (10, 201.2)
  , (11, 201.6)
  , (12, 201.5)
  , (13, 201.5)
  , (14, 203.5)
  , (17, 210.5)
  , (24, 215.1)
  , (25, 218.7)
  ]

file4 :: [(Int, Double)]
file4 =
  [ (26, 219.8)
  , (27, 220.5)
  , (28, 223.8)
  , (29, 222.8)
  , (30, 223.8)
  , (31, 221.7)
  , (32, 222.3)
  , (33, 220.8)
  , (34, 219.4)
  , (35, 220.1)
  , (36, 220.6)
  ]


{-
The data is split across the 4 files. There are overlaps as well. We want to:

  - Stitch the files together
  - Keep track of the missing data
  - Perform analysis on the time series without having to worry about errors due
    to missing values

To stitch two time series together and make a new one, we can make the time
series an instance of Semigroup. To be able to combine time series elements, we
can implement Monoid so we can use mconcat. To handle missing values, we'll use
the Maybe type.
-}

-- Time series type, TS
data TS a = TS [Int] [Maybe a]

showTSPair :: Show a => Int -> Maybe a -> String
showTSPair t (Just x) = mconcat [show t, "|", show x,"\n"]
showTSPair t Nothing  = mconcat [show t, "|NA\n"]

instance Show a => Show (TS a) where
  show (TS ts xs) = mconcat rows
    where rows = zipWith showTSPair ts xs

{-
Next the function createTS takes a list of times and a list of values and
produces a TS with a contiguous time-series of data that handles missing data.
-}

createTS :: [Int] -> [a] -> TS a
createTS ts xs = TS ts' xs'
   where txs = Map.fromList (zip ts xs)
         ts'  = [minimum ts .. maximum ts]
         xs'  = map (\t -> Map.lookup t txs) ts'

-- and a helper function to process the data read in from the files
fileToTS :: [(Int, Double)] -> TS Double
fileToTS rows = createTS ts xs
--  where ts = map fst rows
--        xs = map snd rows
  where (ts, xs) = unzip rows


ts1 :: TS Double
ts1 = fileToTS file1

ts2 :: TS Double
ts2 = fileToTS file2

ts3 :: TS Double
ts3 = fileToTS file3

ts4 :: TS Double
ts4 = fileToTS file4

{-
  ghci> ts
  1|200.1
  2|199.5
  3|199.4
  4|198.9
  5|199.0
  6|200.2
  7|NA
  8|NA
  9|200.3
  10|201.2
  11|NA
  12|202.9
-}

{- Stitching together TS using Semigroup

Two challenges in the implementation that mean we cannot simply append the two
time series:
  1) The ranges of the data points in different files overlap, e.g. file2 has
     a value for date 11 but file1 has one for date 12.
  2) Conflicting data, e.g. file1 and file2 both contain values for date 12, and
     the values are different.

To solve both these problems, we will build a map of the first TS key-value
pairs, then update that map with the key-value pairs from the second TS. This
gives the second TS priority when a duplicate is encountered.
-}

-- Since TS is a list a Maybe values, we need a helper function to insert into
-- the map
insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_, Nothing)      = myMap
insertMaybePair myMap (key, (Just val)) = Map.insert key val myMap

-- The combine function
combineTS :: TS a -> TS a -> TS a
combineTS ts1          (TS [] [])   = ts1
combineTS (TS [] [])   ts2          = ts2
combineTS (TS ts1 xs1) (TS ts2 xs2) = TS ts' xs'
  where ts1Map = foldl insertMaybePair Map.empty (zip ts1 xs1)
        bothTsMap = foldl insertMaybePair ts1Map (zip ts2 xs2)
        bothTs = mconcat [ts1, ts2]
        ts'  = [minimum bothTs .. maximum bothTs]
        xs'  = map (\t -> Map.lookup t bothTsMap) ts'

{-
Here's how the combineTS function works:
  - Handle the two cases where either of the TS are empty
  - For two non-empty TS:
     - build a map of all pairs in ts1 using foldl with insertMaybePair and
       Map.empty to initialise the fold function
     - insert all pairs from ts2 into the map in the same way. The only
       difference is we don't use Map.empty but the map from the previous step
     - combine all the times together into a single list
     - use the combine list of times to find the min and max and generate a
       contiguous list of times
     - create the new list of values in the same way we did in createTS

Note: we could use combineTS directly to implement the Semigroup's (<>)
function. However, the author suggests it is easier to debug a separate function
like this.
-}

instance Semigroup (TS a) where
  (<>) = combineTS  -- we don't even need to add the extra arguments, ts1 ts2

ts1And2 :: TS Double
ts1And2 = ts1 <> ts2

{-
  ghci> ts1And2
  1|200.1
  2|199.5
  3|199.4
  4|198.9
  5|199.0
  6|200.2
  7|NA
  8|NA
  9|200.3
  10|201.2
  11|201.6
  12|201.5
  13|201.5
  14|203.5
  15|204.9
  16|207.1
  17|NA
  18|210.5
  19|NA
  20|208.8
-}

{-
With TS an instance of Semigroup, we can combine time series filling in missing
values and overwriting the duplicate values.

While being able to combine TS is great, we have multiple files to load in. It
would be even better to combine a list of TS. We can do this by implementing
the Monoid type class.
-}

instance Monoid (TS a) where
  mempty = TS [] []
  mappend = (<>)


-- We can now combine all of our time series data
tsAll :: TS Double
tsAll = mconcat [ts1, ts2, ts3, ts4]

{-
  ghci> tsAll
  1|200.1
  2|199.5
  3|199.4
  4|198.9
  5|199.0
  6|200.2
  7|NA
  8|NA
  9|200.3
  10|201.2
  11|201.6
  12|201.5
  13|201.5
  14|203.5
  15|204.9
  16|207.1
  17|210.5
  18|210.5
  19|NA
  20|208.8
  21|NA
  22|NA
  23|NA
  24|215.1
  25|218.7
  26|219.8
  27|220.5
  28|223.8
  29|222.8
  30|223.8
  31|221.7
  32|222.3
  33|220.8
  34|219.4
  35|220.1
  36|220.6
-}

{- Performing calculations on your time series

In this part of the lesson, we'll look at calculating the average (mean) of
the data, as well as finding the highest and lowest values and when they
happened.
-}

mean :: Real a => [a] -> Double
mean xs = total/count
  where total = (realToFrac . sum) xs
        count = (realToFrac . length) xs


meanTS :: Real a => TS a -> Maybe Double
meanTS (TS _ []) = Nothing
meanTS (TS ts xs) = if all (== Nothing) xs
                    then Nothing
                    else Just avg
  where justXs = filter isJust xs
        valXs  = map fromJust justXs
        avg    = mean valXs

{-
  ghci> meanTS tsAll
  Just 210.5966666666667
-}

{-
Next, we'll write the min and max functions. Note, we can write an abstract
function, compare, of type a -> a -> a which is similar to max, i.e. it takes
two values and returns a value of the same type. Note, this is also the same
signature of Semigroup, but we want to compare rather than combine. Another
challenge is again we have Maybe to deal with. Our compare function will need to
handle: (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a).

We'll write a helper function to transform any basic function such as min or
max into the signature above.
-}

type CompareFunc a = (a -> a -> a)
type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

makeCompareTS :: Eq a => CompareFunc a -> TSCompareFunc a
makeCompareTS f = newFunc
  where newFunc (i1, Nothing)   (i2, Nothing)   = (i1, Nothing)
        newFunc (i1, val)       (_ , Nothing)   = (i1, val)
        newFunc (_,  Nothing)   (i2, val)       = (i2, val)
        newFunc (i1, Just val1) (i2, Just val2) = if f val1 val2 == val1
                                                  then (i1, Just val1)
                                                  else (i2, Just val2)

{- Now we can use our function like so:

  ghci> makeCompareTS max (3, Just 200) (4, Just 10)
  (3,Just 200)

Now we can write a generic compareTS that lets us apply comparison functions
like min and max to our TS and get back the value.
-}

compareTS :: Eq a => CompareFunc a -> TS a -> Maybe (Int, Maybe a)
compareTS f (TS [] []) = Nothing
compareTS f (TS ts vs) = if all (== Nothing) vs
                         then Nothing
                         else Just best
  where pairs = zip ts vs
        newFunc = makeCompareTS f
        best = foldl newFunc (0, Nothing) pairs

{-
  ghci> compareTS min tsAll
  Just (4,Just 198.9)
  ghci> compareTS max tsAll
  Just (28,Just 223.8)

We can also define these as dedicated functions:
-}

minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max

{-
  ghci> minTS tsAll
  Just (4,Just 198.9)
  ghci> maxTS tsAll
  Just (28,Just 223.8)
-}

{-
Now we'll move onto more advanced analysis of time series data. Given we have
a time series of our company's monthly sales data, it can be surprisingly tricky
to answer questions such as "How fast are sales growing?". For this we need to
look that the rate of change over time. Another problem is that time series data
is often noisy and requires smoothing to remove noise from the data to make it
easier to understand. Both of these tasks are transformations on the original
data so we can extract insights from it.

First, we'll look at taking the diff, i.e. the difference between each day. We
can define its signature as TS a -> TS a.

TS is parameterised with any type, a. However, we cannot diff any type. We have
to add a constraint to diff to work with Num types. Thus, it becomes:
Num a => TS a -> TS a.

A problem: our diff function expects to work on Num a, but these are wrapped in
Maybe a. As such, we'll need a helper to diff to Maybe types.
-}

diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair Nothing  _        = Nothing
diffPair _        Nothing  = Nothing
diffPair (Just x) (Just y) = Just (x - y)

diffTS :: Num a => TS a -> TS a
diffTS (TS [] []) = mempty
diffTS (TS ts vs) = TS ts (Nothing:diffs)
  where shiftVs = tail vs
        diffs  = zipWith diffPair shiftVs vs

{-
  ghci> diffTS ts1
  1|NA
  2|0.5999999999999943
  3|9.999999999999432e-2
  4|0.5
  5|-9.999999999999432e-2
  6|-1.1999999999999886
  7|NA
  8|NA
  9|NA
  10|-0.8999999999999773
  11|NA
  12|NA

  ghci> meanTS (diffTS tsAll)
  Just (0.6076923076923071)

On average, sales have grown by about 0.6 each month.

Now let's take a look at smoothing. Time series data often has noisy spikes,
unexplainable drops, and other random noise. Another issue is seasonality. If
we have weekly data, will sales be as good on a Sunday as on Tuesday? The
seasonality of the data shouldn't affect how we understand it.

The best way to smooth the data is by taking the moving average. This is similar
to a diff, but instead of looking at two numbers at a time, we're averaging over
an entire window. Our moving average will take a parameter n for the number of
items to smooth over. E.g. if we have a moving average where n=3 for the
sequences:

  Input:  1, 2, 3, 4, 3, 2
  Output: 2.0, 3.0, 3.3, 3.0

The book shows a graph of a moving average where n=12 applied to the sales data.
Notice, that we have n/2 missing values. For diff, we added a single Nothing at
the start, but for moving average, we need to "center" the data by adding NA
(i.e. Nothing) to the beginning and end.
-}

-- helper to deal with Maybe
meanMaybe :: (Real a) => [Maybe a] -> Maybe Double
meanMaybe xs = if all (== Nothing) xs
               then Nothing
               else (Just avg)
  where justXs = filter isJust xs
        vals   = map fromJust justXs
        avg    = mean vals

movingAvg :: (Real a) => [Maybe a] -> Int -> [Maybe Double]
movingAvg []   n = []
movingAvg vals n = if length window == n
                   then (meanMaybe window):(movingAvg rest n)
                   else []
  where window = take n vals
        rest   = tail vals

movingAverageTS :: (Real a) => TS a -> Int -> TS Double
movingAverageTS (TS [] []) n = mempty
movingAverageTS (TS ts vs) n = TS ts smoothedValues
  where ma = movingAvg vs n
        nothings = replicate (n `div` 2) Nothing  -- creates a list of Nothings
        smoothedValues = mconcat [nothings,ma,nothings]  -- to centre the m.a.

{-
  ghci> movingAverageTS tsAll 4
  1|NA
  2|NA
  3|199.475
  4|199.2
  5|199.375
  6|199.36666666666665
  7|199.6
  8|200.25
  9|200.75
  10|201.03333333333333
  11|201.15
  12|201.45
  13|202.025
  14|202.85
  15|204.25
  16|206.5
  17|208.25
  18|209.36666666666667
  19|209.9333333333333
  20|209.65
  21|208.8
  22|208.8
  23|215.1
  24|216.89999999999998
  25|217.86666666666665
  26|218.52499999999998
  27|220.7
  28|221.72500000000002
  29|222.72500000000002
  30|223.02500000000003
  31|222.64999999999998
  32|222.14999999999998
  33|221.04999999999998
  34|220.65
  35|220.22500000000002
  36|NA
-}

{-
Extensions:
  - Using the median for smoothing
  - A function to compute the div (% change) rather than the diff
  - A function to compute the standard deviation
-}
