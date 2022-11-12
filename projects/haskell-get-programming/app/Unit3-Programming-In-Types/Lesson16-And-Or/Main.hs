{- Lesson 16: Creating types with "and" and "or"

In this lesson, we'll be able to:
 - Understand product types
 - Use sum types to model problems
 - Think beyond hierarchical program design

Most of the types we've encountered so far are Algebraic Data Types (ADTs).
An ADT is any type that can be combined to make other ADTs. There are only
two ways to combine types: using AND or OR. E.g. a Name is a String AND
another String. A Bool is a True OR a False data constructor.

Types made with AND are called product types.
Types made with OR are called sum types.
-}


-- Example product types
--data AuthorName = AuthorName {
--  firstName :: String,
--  lastName  :: String
--}
--
--data Book = Book {
--  author :: AuthorName,
--  isbn   :: String,
--  title  :: String,
--  year   :: Int,
--  price  :: Double
--}

{- The curse of product types: hierarchical design

In many programming languages product types are the ONLY way to construct
new types. This unfortunately leads to hierarchical design. As you can only
extend a concept by adding to it, you start with the most abstract thing and
work down: a class hierarchy. This is top-down design.

The example in the book considers the need for our online store to extend its
inventory by adding a VinylRecord in addition to the Book type above. There
are a number of design challenges:
 - The AuthorName type cannot be reused. Sometimes the artist is a band
   rather than an individual
 - Vinyls don't have ISBN numbers

The solution using product types is to create a generic StoreItem base type
that contains all the common properties that can be extended by the two types.
This works, but we have to refactor all the code to use StoreItem and add
conditional logic for the two subtypes.

Later on, we want to add support for CollectibleToy class. We have to refactor
everything again as the base type StoreItem contains properties that don't apply
to CollectibleToy, such as title and year.

Sum Types
We have already seen sum types in action, such as the Name type that had two
data constructors, Name with two Strings and NameWithMiddle with three Strings.
Sum types shouldn't be conflated with simple enum types seen in many languages.
As the Name type shows the data constructors can take arguments and thus be
expressive about what the type means.

Trying to model sum types in Python: you'd probably create a dataclass or impl
a TypedDict for each data constructor and then define a sum type as the union
of them. It gets complicated knowing whether to they should contain a structural
discriminator (e.g. discriminated unions) or if the class name is sufficient
(e.g. nominal typing).

You can see how Haskell's syntax is so much more concise to express sum types.
Let's start by modelling the author and artist types.
-}

type FirstName  = String
type MiddleName = String
type LastName = String

data Name = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  | TwoInitialsWithLast Char Char LastName
  | FirstNameWithTwoInits FirstName Char Char deriving (Show)

data Author = Author Name  deriving (Show)
data Artist = Person Name | Band String  deriving (Show)

-- we can now define the new Creator type to represent an author or an artist
data Creator = AuthorCreator Author | ArtistCreator Artist deriving (Show)

-- we even defined two extra types of Name, TwoInitialsWithLast and
-- FirstNameWithTwoInits. This allows us to support authors like H.P. Lovecraft
-- and bands like Andrew W.K.

hpLovecraft :: Creator
hpLovecraft = AuthorCreator (
                Author (
                  (TwoInitialsWithLast 'H' 'P' "Lovecraft")))

andrewWk :: Creator
andrewWk = ArtistCreator (
            Person (
              (FirstNameWithTwoInits "Andrew" 'W' 'K')))

-- To support such functionality in a hierarchical design would require subtypes
-- for each type of Name. This would likely be infeasible and you'll instead
-- model Name as one big product type. However, that opens many challenges
-- around correctness requiring lots of extra code to ensure the name is in a
-- valid state.

-- Pamphlet type for Q16.1
data Pamphlet = Pamphlet {
  pamphletTitle       :: String,
  pamphletDescription :: String,
  pamphletContact     :: String
}

-- With these new tools we can define our store item types
data Book = Book {
  author    :: Creator,
  isbn      :: String,
  bookTitle :: String,
  bookYear  :: Int,
  bookPrice :: Double
}

data VinylRecord = VinylRecord {
  artist      :: Creator,
  recordTitle :: String,
  recordYear  :: Int,
  recordPrice :: Double
}

data CollectibleToy = CollectibleToy {
  name        :: String,
  description :: String,
  toyPrice    :: Double
}

data StoreItem = BookItem Book
  | VinylRecordItem VinylRecord
  | CollectibleToyItem CollectibleToy
  | PamphletItem Pamphlet


-- Note: a massive short coming of Haskell is that the record syntax generates
-- boilerplate functions for getters, etc. so we get naming collisions for
-- properties with the same name in different records. Because of this, we have
-- to use different names, e.g. bookPrice and recordPrice. There are some
-- proposals to fix it: https://wiki.haskell.org/TypeDirectedNameResolution

-- we can define functions that work on all types
price :: StoreItem -> Double
price (BookItem book)          = bookPrice book
price (VinylRecordItem vinyl)  = recordPrice vinyl
price (CollectibleToyItem toy) = toyPrice toy
price _                        = 0.0

madeBy :: StoreItem -> String
madeBy (BookItem book)         = show (author book)
madeBy (VinylRecordItem vinyl) = show (artist vinyl)
madeBy (PamphletItem pamphlet) = show (pamphletContact pamphlet)
madeBy _                       = "unknown"


-- Questions
-- Q16.1: Create a Pamphlet type that identifies the organisation of the item
--        Add this to every StoreItem

-- I added Pamphlet as a property to each StoreItem. This does seem repetitive.
-- Maybe it would be sensible to introduce a top level product type that
-- composes pamphlet and the item...

-- As it turns out, the question was getting at adding Pamphlet as a new type
-- of StoreItem.

-- Q16.2: Create a Shape type with Circle, Square, and Rectangle shapes. Write
--        functions to compute the perimeter and area of the Shape
type Length = Double
type Area = Double
type Radius = Length
data Shape = Circle Radius | Square Length | Rectangle Length Length

perimeter :: Shape -> Length
perimeter (Circle radius) = 2 * pi * radius
perimeter (Square x)      = 4 * x
perimeter (Rectangle x y) = 2 * (x + y)

area :: Shape -> Area
area (Circle radius) = pi * radius^2
area (Square x)      = x^2
area (Rectangle x y) = x * y


circle :: Shape
circle = Circle 4.0
square :: Shape
square = Square 5.0
rect :: Shape
rect = Rectangle 2.0 3.0

  -- ghci> perimeter circle
  -- 25.132741228718345
  -- ghci> perimeter square
  -- 20.0
  -- ghci> perimeter rect
  -- 10.0
  -- ghci> area circle
  -- 50.26548245743669
  -- ghci> area square
  -- 25.0
  -- ghci> area rect
  -- 6.0
