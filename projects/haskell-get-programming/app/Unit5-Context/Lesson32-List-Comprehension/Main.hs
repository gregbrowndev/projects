import           Control.Applicative
import           Control.Monad
{- Lesson 32: The List Monad and List Comprehensions

After this lesson, we'll be able to:

    - Use do-notation to generate lists
    - Filter results in do-notation by using guard
    - Further simplify do-notation with list comprehensions

In the last lesson, we saw that List is an instance of Monad and how to use this
characteristic to process a list of candidates:

    assessCandidateList :: [Candidate] -> [String]
    assessCandidateList candidates = do
        candidate <- candidates
        let passed = viable candidate
        let statement = if passed
                        then "passed"
                        else "failed"
        return statement

We saw that the <- bind operator allows us to treat the list of candidates like
a single value. The rest of the code looks like it's operating on a single value,
and yet the final result is the same as applying the logic to every candidate in
the list.
-}

{- 32.1 Building lists with the list monad

The main use of the list monad is to generate lists. For example, we can generate
the list of powers of 2:
-}
powersOfTwo :: Int -> [Int]
powersOfTwo n = do
    value <- [1 .. n]
    return (2 ^ value)

{-
The magic of the Monad class is that you can pretend that types in a context
are just plain types.

We could solve this problem using map, but that would be treating list as a
list data structure, not abstracting out the context of the list. For this
example, it may be easier to read and write using map, but as we start to solve
more complicated problems, being able to focus on how you'd transform a single
value can be helpful. Let's look at more examples.
-}

powersOfTwoMap :: Int -> [Int]
powersOfTwoMap n = map (\x -> 2^x) [1 .. n]

powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = do
    value <- [1 .. n]
    let powersOfTwo = 2 ^ value
    let powersOfThree = 3 ^ value
    return (powersOfTwo, powersOfThree)

allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = do
    evenValue <- [2,4 .. n]
    oddValue <- [1,3 .. n]
    return (evenValue, oddValue)

{-
    ghci> allEvenOdds 5
    [(2,1),(2,3),(2,5),(4,1),(4,3),(4,5)]
-}

allSquares :: Int -> [(Int, Int)]
allSquares n = do
    value <- [1 .. n]
    return (value, value^2)

{-
    ghci> allSquares 10
    [(1,1),(2,4),(3,9),(4,16),(5,25),(6,36),(7,49),(8,64),(9,81),(10,100)]
-}

{- 32.1.1 The guard function

Another trick is to filter lists using the list monad. The Control.Monad provides
a function called guard that allows you to filter your values in a list.

Note: we need to import Control.Monad to use the guard function.
-}
evensGuard :: Int -> [Int]
evensGuard n = do
    value <- [1 .. n]
    guard(even value)
    return value

{-
    ghci> evensGuard 10
    [2,4,6,8,10]

If we look at the type signature of guard, we'll see that it has a type class
constraint, Alternative, that we haven't seen before.

    ghci> :i guard
    guard :: GHC.Base.Alternative f => Bool -> f ()
            -- Defined in ‘Control.Monad’

Alternative is a subclass of Applicative but is not a superclass of Monad (
all Applicatives are Alternatives but not all instances of Monad are instances
of Alternative). For the guard function, the key method of Alternative is
empty which works exactly like mempty from Monoid. Lists and Maybe are members
of Alternative. List's empty value is [] and Maybe's is Nothing. However, IO
is not a member of Alternative so you cannot use guard.
-}

{- 32.2 List comprehensions

If we're a Python developer, Haskell's list comprehensions might seem verbose.
For example, this generates a list of even squares:

    [n**2 for n in range(10) if n**2 % 2 == 0]

This in Haskell using do-notation this looks like:
-}
evenSquares :: [Int]
evenSquares = do
    n <- [0 .. 9]
    let nSquared = n ^ 2
    guard(even nSquared)
    return nSquared

-- Wanted to desugar the guard function but got stuck!
--evenSquaresDesugared :: [Int]
--evenSquaresDesugared =
--    [0 .. 9] >>= (\n ->
--        (\nSquared ->
--            guard(even nSquared) >>= (\filtered ->
--                return filtered
--            )
--        )(n ^ 2)
--    )

{-
It may come as a surprise to any Python developer, but list comprehensions are
just specialised application's of monads! List comprehensions are even simpler
for generating lists than do-notations
-}

powersOfTwoLC :: Int -> [Int]
powersOfTwoLC n = [value^2 | value <- [1 .. n]]

powersOfTwoAndThreeLC :: Int -> [(Int, Int)]
powersOfTwoAndThreeLC n = [(powersOfTwo, powersOfThree)
                          | value <- [1 .. n]
                          , let powersOfTwo = 2 ^ value
                          , let powersOfThree = 3 ^ value]

evenSquaresLC :: Int -> [Int]
evenSquaresLC n = [value ^ 2 | value <- [1 .. n], even value]

{-
This is much more like the Python code. We see the final expression at the
beginning of the comprehension separated by a | from the rest of the steps
to achieve it.

List comprehensions are a nice way to make working with the list monad even
easier. They can be found in other languages like Python that only support
basic FP, all you need to implement is >>=, >>, and return.

In this unit's capstone, we're going to take abstracting operators on lists one
step further by creating a SQL-like interface for working with lists. By the end
of this capstone, we'll have taken a deep dive into two ways of thinking in
Monads: IO and List. We'll see how powerful the idea of working in a context can be.

For IO, we work in a context to separate stateful, non-pure code necessary for IO
from the rest of your safe, predictable program logic. For lists, we've seen how
to make generating complex data much easier by using the Monad type class. We've
also seen many examples of using monads to write programs to deal with missing values
while never having to think about how to handle those missing values. All three
contexts are extremely different and yet the Monad type class allows you to think
about them in the exact same way.
-}

{- Summary

In this lesson, we explored how List behaves as a member of Monad. We saw that
list comprehensions, popular in other langauges like Python, are equivalent to
Monads. Any list comp can be converte to do-notation, and do-notation can be
desugared to >>= and lambdas.
-}

{- Q32.1

Use a list comprehension that generates a list of correct calendar dates, given
that you know the number of days in each month. For example, it should start
with 1 .. 31 for January and be followed by 1 .. 28 for February.
-}
daysInMonth :: [Int]
daysInMonth = [31, 28] -- 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

calendarDates :: [(Int, Int)]
calendarDates = [(month, day)
                | (month, numDays) <- zip [1 ..] daysInMonth
                , day <- [1 .. numDays]
                ]

{-
    ghci> calendarDates
    [(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9),(1,10),(1,11),(1,12),(1,13),(1,14),(1,15),(1,16),(1,17),(1,18),(1,19),(1,20),(1,21),(1,22),(1,23),(1,24),(1,25),(1,26),(1,27),(1,28),(1,29),(1,30),(1,31),
     (2,1),(2,2),(2,3),(2,4),(2,5),(2,6),(2,7),(2,8),(2,9),(2,10),(2,11),(2,12),(2,13),(2,14),(2,15),(2,16),(2,17),(2,18),(2,19),(2,20),(2,21),(2,22),(2,23),(2,24),(2,25),(2,26),(2,27),(2,28),
     ...]
-}

{- Q32.2

Translate the preceding question into do-notation, and then into Monad methods
and lambdas.
-}
calendarDatesDo :: [(Int, Int)]
calendarDatesDo = do
    (month, numDays) <- zip [1 ..] daysInMonth
    day <- [1 .. numDays]
    return (month, day)

calendarDatesM :: [(Int, Int)]
calendarDatesM = do
    (zip [1 ..] daysInMonth) >>= (\(month, numDays) ->
            [1 .. numDays] >>= (\day -> return (month, day))
        )
