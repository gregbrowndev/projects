import qualified Data.Map as Map

{- Lesson 27 - The Functor Type Class

After this lesson, we'll be able to:

    - Use the Functor type class
    - Solve problems with fmap and <$>
    - Understand kinds for Functors


The intro to this chapter is worth reading in full. I don't want to
rewrite the whole thing, but the gist is that there are three possible
types of mapping that crop up when working with functions that expect no
contexts.

For example, we have function A -> B. However, in our program,
we want to use this function but we only have a Maybe A. Clearly, we
can wrap the function so it becomes Maybe A -> Maybe B. This is what
the Functor type class does for us.

The Functor type class provides a generic interface for applying
functions to values in a container or context. E.g. if we have the types:

    - [Int]
    - Map String Int
    - Maybe Int
    - IO Int

All four types are different but parameterised by the same type Int
(Map is a special case, but its value is Int). Now if we have a function:
Int -> String. In most programming languages, we'd need a separate version
of this function that works with each of the four types above. However, with
the Functor type class, we get a uniform way to apply the same function in
all cases.
-}

{- Using functions in context with the Functor type class

Let's look at an example. We want a function to increment an integer
but the integer may be missing. We might write:

    incMaybe :: Maybe Int -> Maybe Int
    incMaybe (Just n) = Just (n + 1)
    incMaybe Nothing = Nothing

This code scales horribly. It turns out Haskell has a solution for this,
Maybe is a member of the Functor type class. Functor requires only one
definition, fmap, i.e. "functor map".

    fmap :: Functor f => (a -> b) -> f a -> f b

The signature shows that f is constrained to be a member of the Functor
type class, the first arg is a function from type a to type b, the second
arg is a Functor of type a (e.g. Maybe Int), and the last return arg is
the transformed Functor of type b (e.g. Maybe Double).

The fmap function (and the analogous <$> binary operator), provide an
adapter for working with a pure function within some context.

The definition that makes Maybe an instance of Functor looks like:

    instance Functor Maybe where
        fmap func (Just n) = Just (func n)
        fmap func Nothing  = Nothing

With fmap, we can easily use a pure function while keeping our value in
a Maybe context:
-}

request :: Maybe Int
request = Just 200

-- GHCi> fmap (+ 1) request
-- Just 201

{-
The (+1) adds one to the Maybe Int and returns a Maybe Int

Lets look at an example using Functor to solve four separate problems
in the same way
-}

data RobotPart = RobotPart {
    name        :: String,
    description :: String,
    cost        :: Double,
    count       :: Int
} deriving Show

leftArm :: RobotPart
leftArm = RobotPart {
    name = "left arm",
    description = "left arm for face punching",
    cost = 1000.0,
    count = 3
}

rightArm :: RobotPart
rightArm = RobotPart {
    name = "right arm",
    description = "right arm for nice gestures",
    cost = 1025.0,
    count = 5
}

robotHead :: RobotPart
robotHead = RobotPart {
    name = "robot head",
    description = "this head looks mad",
    cost = 5092.25,
    count = 2
}

-- we want to render the information as HTML
type Html = String

renderHtml :: RobotPart -> Html
renderHtml part = mconcat [
        "<h2>",partName, "</h2>"
        ,"<p><h3>desc</h3>",partDesc
        ,"</p><p><h3>cost</h3>"
        ,partCost
        ,"</p><p><h3>count</h3>"
        ,partCount,"</p>"
    ]
    where partName = name part
          partDesc = description part
          partCost = show (cost part)
          partCount = show (count part)

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
    where keys = [1, 2, 3]
          vals = [leftArm, rightArm, robotHead]
          keyVals = zip keys vals

-- Map is a useful example because it's involves use of three Functors,
-- it's made from a List, returns Maybe values, and is itself a Functor.

-- Converting a Maybe RobotPart to Maybe Html

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

-- Using the binary operator form of fmap
partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

-- Converting a list of parts into a list of Html snippets
allParts :: [RobotPart]
allParts = map snd (Map.toList partsDB)

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts

-- Note, since <$> is just fmap and fmap for lists is just map, we could also write:
allPartsHtml2 :: [Html]
allPartsHtml2 = map renderHtml allParts

-- Instead of having a Map of robotParts, we may instead want a Map of Html
-- snippets so we don't have to rerender the Html each time. This is easy
-- since Map is an instance of Functor.
htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

-- In the final example, we'll transform a IO RoboPart into IO Html
leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO

{- Summary

Let's review all the transformations that the Functor type provides:

    partHtml :: Maybe Html
    partHtml = renderHtml <$> partVal

    allPartsHtml :: [Html]
    allPartsHtml = renderHtml <$> allParts

    htmlPartsDB :: Map.Map Int Html
    htmlPartsDB = renderHtml <$> partsDB

    htmlSnippet :: IO Html
    htmlSnippet = renderHtml <$> leftArmIO

Functor provides a common interface to apply any function to a value in a
context. For types like List and Map, this is a convenient way to update
values in these containers. For IO, it's essential to be able to change values
in an IO context, because you can't take IO values out of their context.
-}


{-
Q27.3 Write a command-line interface for partsDB that lets the user look up the cost of
an item, given an ID. Use the Maybe type to handle the case of the user entering missing
input.
-}
getCost :: Map.Map Int RobotPart -> Int -> Maybe Double
getCost partsDB partId = cost <$> part
    where part = Map.lookup partId partsDB

printCost :: Maybe Double -> IO ()
printCost Nothing     = putStrLn "item not found"
printCost (Just cost) = print cost

main :: IO ()
main = do
    putStrLn "enter a part number"
    partNo <- getLine
    let cost = getCost partsDB (read partNo)
    printCost cost

{-
    ghci> main
    enter a part number
    1
    1000.0
    ghci> main
    enter a part number
    6
    item not found
-}
