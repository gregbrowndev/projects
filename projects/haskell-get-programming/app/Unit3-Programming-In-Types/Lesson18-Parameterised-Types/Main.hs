import qualified Data.Map as Map

{- Lesson 18 - Parameterised Types

In this lesson, we'll:
  - use parameterised types to make generic data types
  - understand kinds of types
  - write code using the Data.Map type to look up values

A parameterised type is a type that takes arguments. They are important as they
allow us to place constraints between the arguments, e.g. a pair of the same
type. Parameterised types are like generics in other languages (although the
book once again hints at differences without explaining them, at least yet).

Here's a simple example:
-}

data Box a = Box a deriving (Show)

{-
  ghci> n = 6 :: Int
  ghci> :t Box n
  Box n :: Box Int
  ghci> word = "box"
  ghci> :t Box word
  Box word :: Box String
-}

-- we can also make parameterised (generic) functions for this new type:

wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x

-- Another more useful example:
data Triple a = Triple a a a
  deriving (Show, Eq)

-- newtype seems to work quite a bit differently to data. Tt requires you pass a
-- tuple, e.g. triple = Triple (1, 2, 3), instead of multiple args like data
--newtype Triple a = Triple (a, a, a)
--  deriving (Show, Eq)

type Point3D = Triple Double
aPoint :: Point3D
aPoint = Triple 0.1 53.2 12.3

type FullName = Triple String
aPerson :: FullName
aPerson = Triple "Howard" "Phillips" "Lovecraft"

-- Working with Triples
-- Note: fst and snd only work on 2-tuples
first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ y _) = y

third :: Triple a -> a
third (Triple _ _ z) = z

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

{-
  ghci> transform (* 3) aPoint
  Triple 0.30000000000000004 159.60000000000002 36.900000000000006

  ghci> transform reverse aPerson
  Triple "drawoH" "spillihP" "tfarcevoL"
-}

{- Lists

The List type is a generic container like Triple, but it has some special built
in syntax, the parentheses List(), for convenience that you cannot emulate.

Looking at the definition of List is interesting:

  ghci> :i []
  type [] :: * -> *
  data [] a = [] | a : [a]

This shows `[]` is a type parameterised by a and it has two data constructors:
1. `[]`, i.e. a literal empty list and 2. `a : [a]`, i.e. a value `a` consed
with another list of type `a`. This definition is recursive. Amazingly, this is
the complete definition of a List in Haskell.

We can create our own List type.
-}

data MyList a = Empty | Cons a (MyList a)
  deriving (Show)

-- Compare this to the definition of MyList in the Udemy Scala course (holy f)
-- These are identical to the built in List:

{-
  ghci> ex1a = 1:2:3:[]
  ghci> ex1b = Cons 1 (Cons 2 (Cons 3 Empty))

  ghci> ex2a = 'c':'a':'t':[]
  ghci> ex2b = Cons 'c' (Cons 'a' (Cons 't' Empty))
-}

-- we can implement map for MyList
myMap :: (a -> b) -> MyList a -> MyList b
myMap _ Empty            = Empty
myMap f (Cons head tail) = Cons (f head) (myMap f tail)

{-
  ghci> myMap (*2) ex1b
  Cons 2 (Cons 4 (Cons 6 Empty))
-}

{- Types with more than one type parameter

A tuple is the most common and only multi-parameter type we have seen so far.
To see the definition of a 2-tuple:

  ghci> :info (,)
  type (,) :: * -> * -> *
  data (,) a b = (,) a b


Kinds: The type of a type is called its kind. They're abstract and covered in
more detail in unit 5 (Functor, Applicative, and Monad). The kind of a type
indicates the number of parameters the type takes which are expressed by an
asterisk (*). A type with:

  - no params has kind   *
  - one param has kind   * -> *
  - two params has kind  * -> * -> *
  - etc.

We can use the :kind command to look up a type's kind:

  ghci> :kind Int
  Int :: *
  ghci> :kind []
  [] :: * -> *
  ghci> :kind (,)
  (,) :: * -> * -> *
  ghci> :kind Map.Map
  (,) :: * -> * -> *

Concrete types have a different kind to their nonconcrete equivalents:

  ghci> :kind [Int]
  [Int] :: *
  ghci> :kind Triple Char
  Triple Char :: *

We'll need to understand kinds to better understand Functors and Monads (unit 5)

Section 18.2.3: Data.Map

At the top of the file, we'll done a qualified import: like an alias.

  import qualified Data.Map as Map

Note: I also had to add the containers dependency in package.yaml:

  dependencies:
  - base >= 4.7 && < 5
  - containers

Unlike Lists and Tuples, the Map impl is non-trivial. We'll look at an example:
creating a monster from an inventory of organs.
-}

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

-- Maps vs Hash Table: They are similar but differ in how they look up values.
-- A hash table uses a function to transform the key into an index of an array
-- where the value is stored. This allows extremely fast look up but requires
-- more memory to store in order to prevent collisions. A map on the other hand
-- uses a binary search tree to look up values. This is slower (but still fast)
-- and uses less memory. In order to impl a binary search tree, the keys must
-- belong to the Ord class.

-- Each organ is filed in a drawer with an ID. Here are the associated IDs for
-- each organ:
ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

{- With this we can build a map. One way to do this is using Map.fromList:

  ghci> :t Map.fromList
  Map.fromList :: Ord k => [(k, a)] -> Map.Map k a

Here we can see the restriction that k must be in Ord.
-}

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

{-
  ghci> organCatalog
  fromList [(2,Heart),(7,Heart),(13,Brain),(14,Spleen),(21,Spleen),(24,Kidney)]
  ghci> Map.lookup 7 organCatalog
  Just Heart

The Map.lookup function returns the organ. But it is prefixed with "Just". We
can see why by looking at its type signature:

  ghci> :t Map.lookup
  Map.lookup :: Ord k => k -> Map.Map k a -> Maybe a

Here we encounter a new parameterised type: Maybe.Maybe.
-}
