import qualified Data.Map as Map
{- Lesson 30 - Introducing the Monad Type Class

After this lesson, I'll be able to:

    - Understand the limitations of both Functor and Applicative
    - Use Monad's (>>=) operator to chain together functions in a context
    - Write IO code without do-notation

We've finished learning about two important type classes, Functor and
Applicative, that allow you to perform powerful computations within a
context. Functor allows you to change individual values in a context:

    GHCi> (+2) <$> Just 3
    Just 5

Applicative allows you to use partial application in a context which allows
you to pass multiple arguments in a context:

    GHCi> pure (+) <*> Just 3 <*> Just 2
    Just 5

In this lesson, we'll look at the final evolution of this process, the Monad
type class. The Monad type class allows you to perform any arbitrary computation
in a context you'd like. We already saw this in action with the do-notation, which
is just syntactic sygar for the methods of the Monad type class.

    main :: IO ()
    main = do
        putStrLn "Remember do-notation!"
        putStrLn "It makes things easy!"

We'll start this lesson by looking at two relatively straightforward problems
that are frustratingly challenging to solve with Functor and Applicative. Then
we'll learn about Monad's bind operator. We'll finish the lesson by using Monad's
methods to write IO actions similar to what we used do-notation for in unit 4.

So far we've solved 3 of the 4 mismatches:

    1. Functor's fmap provides an adapter or when you have a value in a context
       and a regular function, and you want your result in a context.
    2. Applicative's <*> allows you to connect a function in a context with values
       in a context.
    3. Applicative's pure method allows you to handle the case of your final result
       not being in a context (when can always put a result into a context)

The last problem to solve is when the initial argument isn't in a context but its
result is.
-}

{- 30.1.1 Combining two Map lookups

A common problem: needing to look up a value in one Map in order to access another
value in a second Map. E.g. looking up a zip code to find a city, then looking up
the city to find the state.

In this example, we're writing code for managing user credits for a mobile gaming
platform. Currently, each user is identified as a unique GamerId that's just an Int.
The credit system has a legacy dependency where each user's credits are identified
by Username. Therefore, in order to find credits, we need to lookup the user using
their GamerId and then look up their credits using their Username.
-}

type UserName = String
type GamerId = Int
type PlayerCredits = Int

userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList  [(1,"nYarlathoTep")
                           ,(2,"KINGinYELLOW")
                           ,(3,"dagon1997")
                           ,(4,"rcarter1919")
                           ,(5,"xCTHULHUx")
                           ,(6,"yogSOThoth")]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [("nYarlathoTep",2000)
 ,("KINGinYELLOW",15000)
 ,("dagon1997",300)
 ,("rcarter1919",12)
 ,("xCTHULHUx",50000)
 ,("yogSOThoth",150000)]

