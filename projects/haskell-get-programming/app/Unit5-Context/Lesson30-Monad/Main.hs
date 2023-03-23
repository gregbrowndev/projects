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
userNameDB = Map.fromList  [
     (1,"nYarlathoTep")
    ,(2,"KINGinYELLOW")
    ,(3,"dagon1997")
    ,(4,"rcarter1919")
    ,(5,"xCTHULHUx")
    ,(6,"yogSOThoth")]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [
     ("nYarlathoTep",2000)
    ,("KINGinYELLOW",15000)
    ,("dagon1997",300)
    ,("rcarter1919",12)
    ,("xCTHULHUx",50000)
    ,("yogSOThoth",150000)]

lookupUserName :: GamerId -> Maybe UserName
lookupUserName gamerId = Map.lookup gamerId userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB

{-
We want to be able to chain together lookupUserName and lookupCredits. However,
lookupUserName returns a Maybe UserName while lookupCredits just takes a UserName.
Clearly, we need a function with a signature:

    Maybe UserName -> (UserName -> Maybe PlayerCredits) -> Maybe PlayerCredits

If we abstract this out, we get:

    Applicative f => f a -> (a -> f b) -> f b

Don't confuse this with the signature for <*>:

    (<*>) :: f (a -> b) -> f a -> f b

There isn't anything we can do with the functions provided by Functor or
Applicative:

    (<$>) :: Functor f => (a -> b) -> f a -> f b
    (<*>) :: Applicative f => f (a -> b) -> f a -> f b
    pure  :: Applicative f => a -> f a

Instead, we would need to write a wrapper around lookupCredits:
-}

altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
altLookupCredits Nothing         = Nothing
altLookupCredits (Just username) = lookupCredits username

creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId gamerId = altLookupCredits (lookupUserName gamerId)

{-
We can use the wrapper in the creditsFromId function above

Interestingly, we might try to write:

    creditsFromIdStrange id = pure lookupCredits <*> lookupUserName id

However, this produces a strange type signature:

    creditsFromIdStrange :: GamerId -> Maybe (Maybe PlayerCredits)

We end up with a nested Maybe!

This problem doesn't seem so bad. It was easy to write a wrapper around the
Maybe context using pattern matching to handle the two cases. However, other
contexts such as IO, aren't so user friendly. Let's look at a problem were
we try to compose two IO actions to write a function echo:

    getLine :: IO String
    putStrLn :: String -> IO ()
    echo :: IO ()  --  we want to write this function

Clearly, we need a function that combines getLine and putStrLn and returns
IO ():

    IO String -> (String -> IO ()) -> IO ()

Abstracting this out we get:

    Applicative f => f a -> (a -> f b) -> f b

This is the same signature as the Maybe example. To solve this problem, we need
something more powerful than either Functor or Applicative. This brings us to
the Monad type class!
-}

{- 30.2 The bind operator >>=

The missing operator is >>= (pronounced bind):

    (>>=) :: Monad m => m a -> (a -> m b) -> m b

The bind operator has exactly the signature we were looking for. Both Maybe and
IO are instances of Monad which means we can use >>= to solve our problems.
-}
creditsFromId2 :: GamerId -> Maybe PlayerCredits
creditsFromId2 gamerId = lookupUserName gamerId >>= lookupCredits

{-
    ghci> creditsFromId2 1
    Just 2000
-}

echo :: IO ()
echo = getLine >>= putStrLn

{-
    ghci> echo
    Hello world
    Hello world
-}

-- Quick check 30.3
readInt :: IO Int
readInt = read <$> getLine

printDouble :: Int -> IO ()
printDouble n = print (n * 2)

readAndPrint :: IO ()
readAndPrint = readInt >>= printDouble

{-
    ghci> readAndPrint
    5
    10
-}

{- 30.3 The Monad Type Class

The Monad type type class extends Applicative.

    class Applicative m => Monad (m :: * -> *) where
    -- From Functor
      fmap   :: Functor f :: (a -> b) -> f a -> f b
      (<$>)  :: Functor f :: (a -> b) -> f a -> f b
    -- From Applicative
      (<*>)  :: Applicative f :: f (a -> b) -> f a -> f b
      pure   :: Applicative f :: a -> f a
    -- From Monad
      (>>=)  :: Monad m :: m a -> (a -> m b) -> m b
      (>>)   :: Monad m :: m a -> m b -> m b
      return :: Monad m :: a -> m a
      fail   :: Monad m :: String -> m a

The only method required for the minimum definition of Monad is >>=.

The fail method handles the case of errors happening in your Monad. For Maybe,
fail returns Nothing, for IO, fail raises I/O error. We'll discuss fail more in
unit 7 when handling errors in Haskell.

The return method should look familiar. It is identical to pure but has a
different name for historical purposes since the Monad type class predates
Applicative. You could stick to using pure everywhere since it will work for
both Applicatives and Monads, but typically the community uses return when
working with Monads and pure when working with Applicatives.

The >> operator has a strange signature that looks like it throws away the first
argument. It turns out this is exactly what it does. This is useful in contexts
that produce side effects such as IO (there are other discussed in unit 7).
Whenever we use putStrLn, we don't get anything back. It's common we want to
print something to the user and throw away the IO () result. We can modify our
echo function to demonstrate this:
-}

echoVerbose :: IO ()
echoVerbose = putStrLn "Enter a String and we'll echo it!" >>
              getLine >>= putStrLn

{- 30.3.1 Using Monad to build a Hello <name> program

-}
askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloMain :: IO ()
helloMain = askForName
            >> getLine
            >>= (\name -> return (nameStatement name))
            >>= putStrLn
{-
We can chain together askForName with getLine using >> to throw away the IO ().
However, now we need to pass IO String to (String -> String). We can use >>=
but we would need to change makeStatement to return an IO String.

A common solution is to wrap nameStatement in a lambda and use return at the end.
Haskell's type inference will figure out which context to put the result into.

Finally we use >>= putStrLn to write the result to the console.

    ghci> helloMain
    What is your name?
    Greg
    Hello, Greg!
-}

{- Summary

In this lesson, we were introduced to the Monad type class, the final refinement
of computing in a context that we started with Functor. The most important method
is the >>= (pronounced bind) operator.

You use >>= to chain together functions of type (a -> m b). This is particularly
important for the IO context since we cannot use pattern matching to access the
value inside like we can with the Maybe context.
-}

{- Q30.1

To prove that Monad is strictly more powerful than Functor, write a universal
version of <$>, as in the preceding lesson’s exercise, called allFmapM, that
defines <$> for all members of the Monad type class. Because it works for all
instances of Monad, the only functions you can use are the methods required by
the Monad type class (and lambda functions). To get you started, here’s your
type signature:

    allFmapM :: Monad m => (a -> b) -> m a -> m b
-}
allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM f ma = ma >>= (\a -> return (f a))

{-
    ghci> allFmapM (+1) (Just 5)
    Just 6
    ghci> allFmapM (+1) Nothing
    Nothing
-}

{- Q30.2

To prove that Monad is strictly more powerful than Applicative, write a universal
version of <*>, called allApp, that defines <*> for all members of the Monad type
class. Because it works for all instances of Monad, the only functions you can
use are the methods required by the Monad type class (and lambda functions). To
get you started, here’s your type signature:

    allApp :: Monad m => m (a -> b) -> m a -> m b
-}
allApp :: Monad m => m (a -> b) -> m a -> m b
allApp mf ma = mf >>= (\f -> f <$> ma)
-- Note: we should replace <$> with allFmapM to answer the question fully

{-
    ghci> allApp (pure (+1)) (Just 5)
    Just 6
-}

{- Q30.3

Implement a bind function which is the same as (>>=) for Maybe:

    bind :: Maybe a -> (a -> Maybe b) -> Maybe b
-}
bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _  = Nothing
bind (Just a) f = f a
