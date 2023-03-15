import qualified Data.Map as Map

{- Lesson 28 - A Peak at the Application Type Class:
    Using Functions in a Context

After this lesson, I'll be able to:

    - Build an application that missing data
    - Extend the power of the Functor type class with the Applicative type
    - Use Applicative to use one data model in many contexts

In the last lesson, we saw how Functor allows us to apply a function to a value
inside a container (e.g. List or the values of a Map) or a context (e.g. Maybe and
IO). In this lesson, we'll work with a more powerful type class called Applicative
that allows you to use functions that are themselves in a context.

This may not seem that useful, but it allows us to chain together long sequences
of computation in a context such as IO or Maybe.

In the next example, we see the limitations of Functor by building a CLI tool that
allows users to calculate the distance between two cities. Since the user enters
the name of each city, and those cities might not exist, the issue is that you
need to pass two Maybe values to a function, which surprisingly Functor can't do.
We'll see how Applicative resolves this issue.
-}

type LatLong = (Double, Double)

locationDB :: Map.Map String LatLong
locationDB = Map.fromList [("Arkham",(42.6054,-70.7829))
                          ,("Innsmouth",(42.8250,-70.8150))
                          ,("Carcosa",(29.9714,-90.7694))
                          ,("New York",(40.7776,-73.9691))]

-- We need some code to calculate distance between two points on the globe.
-- We don't need to understand it...
toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double,Double)
latLongToRads (lat,long) = (rlat,rlong)
 where rlat = toRadians lat
       rlong = toRadians long

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
 where (rlat1,rlong1) = latLongToRads coords1
       (rlat2,rlong2) = latLongToRads coords2
       dlat = rlat2 - rlat1
       dlong = rlong2 - rlong1
       a = (sin (dlat/2))^2 + cos rlat1 * cos rlat2 * (sin (dlong/2))^2
       c = 2 * atan2 (sqrt a) (sqrt (1-a))
       earthRadius = 3961.0

{-
    ghci> haversine (40.7776,-73.9691) (42.6054,-70.7829)
    207.3909006336738
-}

printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Error, invalid city entered"
printDistance (Just distance) = putStrLn (show distance ++ " miles")

{-
What we need now is a function with the signature:

    Maybe LatLong -> Maybe LatLong -> Maybe Double

However, this is almost exactly the signature of haversine. A naive solution
would be to wrap haversine to work with Maybe:
-}
haversineMaybe :: Maybe LatLong -> Maybe LatLong -> Maybe Double
haversineMaybe Nothing _ = Nothing
haversineMaybe _ Nothing = Nothing
haversineMaybe (Just val1) (Just val2) = Just (haversine val1 val2)

{-
However, like we did we Functor, we want to leverage type classes so we can
apply a pure, simple function within a context.

Let's have a go at solving this with Functor. Maybe using partial application so
we can fmap one argument at a time?

    myDistance :: Maybe Double
    myDistance = (haversine <$> city1) <$> city2
        where city1 = Map.lookup "Arkham" locationDB
              city2 = Map.lookup "New York" locationDB

This doesn't quite work but it is close! If we had a normal value of LatLong
for New York, we could easily use partial application to write a function,
distanceFromNY, that accepts another LatLong. So the solution should look
something similar to that.

The real limitation of Functor's fmap is that if you end up with a function in a
context, through partial application, you have no way of using that function.
-}
maybeInc :: Maybe (Integer -> Integer)
maybeInc = (+) <$> Just 1

{-
The function above does partial application of (+) which takes two
values, by using <$> on a Maybe value, you created a function waiting for a missing
value but the whole function is now in a Maybe context.

Note: the function above is easier to understand by expanding out the partial app:

    maybeInc = (\val -> (+) val) <$> Just 1
             == Just ((\val -> (+) val) 1)
             == Just (\val -> val + 1)

Now we can see why the signature of maybeInc is Maybe (Integer -> Integer).
So we now have this function in a context, but there is no way for us to apply it.

The solution is to use <*> (pronounced "app") for partial application in a context.
We now need to generalise Functor's fmap to work with multiple arguments. In
lesson 5, we learned that partial application means that calling an function with
fewer args than it requires results in a function waiting for the remaining args.
Then in section 10.2.2, we saw that all functions are functions of one argument.
Functions with multiple args are just a chain of single-argument functions. The
key to solving this problem is being able to perform partial application in a
context such as Maybe or IO!

    ghci> maybeInc <*> Just 5
    Just 6
    ghci> maybeInc <*> Nothing
    Nothing
    ghci> maybeInc <*> Just 100
    Just 101

The <*> operator takes a function in a context, e.g. Maybe (Integer -> Integer), and
an argument in the same context, e.g. Just 5, and applies the function to the
argument, returning a type still in the context, e.g. Just 6. We see it's definition:

    type Applicative :: (* -> *) -> Constraint
    class Functor f => Applicative f where
      ...
      (<*>) :: f (a -> b) -> f a -> f b


We now have a general way to use existing binary functions in a Maybe context!

    ghci> (++) <$> Just "cats" <*> Just " and dogs"
    Just "cats and dogs"
    ghci> (++) <$> Nothing <*> Just " and dogs"
    Nothing
    ghci> (++) <$> Just "cats" <*> Nothing
    Nothing

We can use <$> and <*> to chain together any number of arguments. We can use any
other binary operator too:

    ghci> (*) <$> Just 10 <*> Just 4
    Just 40
    ghci> (*) <$> Just 10 <*> Nothing
    Nothing
    ghci> (div) <$> Just 4 <*> Just 2
    Just 2
    ghci> (mod) <$> Just 15 <*> Just 5
    Just 0

Let's use our new knowledge of <*> to finish the city distance program. We can
finally solve the problem of wanting to use the haversine function with two
Maybe values:

    ghci> startingCity = Map.lookup "Carcosa" locationDB
    ghci> destCity = Map.lookup "Innsmouth" locationDB
    ghci> haversine <$> startingCity <*> destCity
    Just 1415.7942372467567
-}

main :: IO ()
main = do
    putStrLn "Enter the start city"
    startCityName <- getLine
    putStrLn "Enter the destination city"
    destCityName <- getLine
    let startCity = Map.lookup startCityName locationDB
    let destCity = Map.lookup destCityName locationDB
    let distance = haversine <$> startCity <*> destCity
    printDistance distance

{-
    ghci> main
    Enter the start city
    Arkham
    Enter the destination city
    New York
    207.3909006336738 miles

    ghci> main
    Enter the start city
    New York
    Enter the destination city
    Hull
    Error, invalid city entered

This shows the value of Functors and Applicatives. We've written a program that
handles missing values well, but not once did we have to check whether a value was
null using conditionals or worry about exception handling. We can write the core
functionality of our program, haversine, as though nothing in the world might go
wrong.
-}

{- Using a multi-argument function in IO using <$> and <*>

IO is also a member of Applicative, we'll write a simple program to return the
minimum of three numbers entered by the user:
-}

minOfThree :: Ord a => a -> a -> a -> a
minOfThree val1 val2 val3 = min val1 (min val2 val3)

-- next create a simple IO action to read an Int from the command line:
readInt :: IO Int
readInt = read <$> getLine

-- now use <$> and <*> to make an IO action that reads in three Ints and returns
-- the minimum
minOfInts :: IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt

main2 :: IO ()
main2 = do
    putStrLn "Enter three numbers"
    minInt <- minOfInts
    putStrLn (show minInt ++ " is the smallest")

{-
    ghci> main2
    Enter three numbers
    5
    2
    8
    2 is the smallest
-}

-- Quick check: use minOfThree to get the minimum value of three Maybe values:
-- ghci> minOfThree <$> Just 10 <*> Just 3 <*> Just 6
-- Just 3

{- 28.3: Using <*> to create data in a context

One of the most common uses of Applicatives in practice is when you want to create
data but all the information you need are in contexts suh as Maybe and IO. E.g.
suppose we have user data for a video game:
-}
data User = User {
    name :: String,
    gamerId :: Int,
    score :: Int
} deriving Show

-- lets say we receive these values from the server or DB which might be missing:
serverName :: Maybe String
serverName = Just "Sue"

serverGamerId :: Maybe Int
serverGamerId = Just 1337

serverScore :: Maybe Int
serverScore = Just 9001

-- To create a user from this data, we can just chain <$> and <*> because the
-- data constructor User works as function that takes three arguments.
myUser :: Maybe User
myUser = User <$> serverName <*> serverGamerId <*> serverScore

-- We can create a user from IO types too:
main3 :: IO ()
main3 = do
    putStrLn "Enter a username, gamerId, and score"
    user <- User <$> getLine <*> readInt <*> readInt
    print user

{-
    ghci> main3
    Enter a username, gamerId, and score
    greggles
    191
    -5
    User {name = "greggles", gamerId = 191, score = -5}
-}

{- Summary

This lesson showed us how the Applicative's <*> operator allows us to use functions
that are themselves in a context, apply them to values in the same context, and get
back a return value in the same context. The <*> operator is essential to extending
Functor to work with multi-argument functions.

Because of the prevalence of partial application in Haskell, it is fairly common
to end up with a function in a context.
-}

{- Questions

Q28.1 Writing haversineMaybe (listing 28.4) was straightforward. Write the function
haversineIO without using <*>.
-}

haversineIO :: IO LatLong -> IO LatLong -> IO Double
haversineIO latLongIO1 latLongIO2 = do
    latLong1 <- latLongIO1
    latLong2 <- latLongIO2
    let distance = haversine latLong1 latLong2
    return distance

{-
Q28.2 Rewrite haversineIO, this time using <*>.
-}

haversineIO2 :: IO LatLong -> IO LatLong -> IO Double
haversineIO2 ioVal1 ioVal2 = haversine <$> ioVal1 <*> ioVal2

{-
Q28.3 Recall the RobotPart type from the preceding lesson:
data RobotPart = RobotPart
 { name :: String
 , description :: String
 , cost :: Double
 , count :: Int
 } deriving Show

Make a command-line application that has a database of various RobotParts (at least five),
and then lets the user enter in two-part IDs and returns the one with the lowest cost.
Handle the case of the user entering an ID that’s not in the parts database.
-}

data RobotPart = RobotPart {
    pName :: String,
    description :: String,
    cost :: Double,
    count :: Int
} deriving Show

leftArm :: RobotPart
leftArm = RobotPart {
    pName = "left arm",
    description = "left arm for face punching",
    cost = 1000.0,
    count = 3
}

rightArm :: RobotPart
rightArm = RobotPart {
    pName = "right arm",
    description = "right arm for nice gestures",
    cost = 1025.0,
    count = 5
}

robotHead :: RobotPart
robotHead = RobotPart {
    pName = "robot head",
    description = "this head looks mad",
    cost = 5092.25,
    count = 2
}

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
    where keys = [1, 2, 3]
          vals = [leftArm, rightArm, robotHead]
          keyVals = zip keys vals


cheapestPart :: RobotPart -> RobotPart -> RobotPart
cheapestPart part1 part2 = if price1 <= price2
                           then part1 else part2
    where price1 = cost part1
          price2 = cost part2

printCost :: Maybe Double -> IO ()
printCost Nothing = putStrLn "missing item"
printCost (Just cost) = print cost

mainRoboQuery :: IO ()
mainRoboQuery = do
    putStrLn "Enter two robopart IDs"
    partId1 <- readInt
    let part2 = Map.lookup partId2 partsDB
    partId2 <- readInt
    let part1 = Map.lookup partId1 partsDB
    let cheaperPart = cheapestPart <$> part1 <*> part2
    printCost (cost <$> cheaperPart)

{-
    ghci> mainRoboQuery
    Enter two robopart IDs
    1
    2
    1000.0

    ghci> mainRoboQuery
    Enter two robopart IDs
    2
    3
    1025.0

    ghci> mainRoboQuery
    Enter two robopart IDs
    1
    3
    1000.0
-}