{-
Q27.1 When we introduced parameterized types in lesson 15, you used
      a minimal type Box as the example:

    data Box a = Box a deriving Show

    Implement the Functor type class for Box. Then implement morePresents, which changes a
    box from type Box a to one of type Box [a], which has n copies of the original value in the
    box in a list. Make sure to use fmap to implement this
-}

data Box a = Box a deriving Show

instance Functor Box where
    fmap func (Box val) = Box (func val)

makeCopies :: a -> Int -> [a]
makeCopies a 0 = []
makeCopies a n = [a] ++ makeCopies a (n - 1)

morePresents :: Box a -> Int -> Box [a]
morePresents box n = (\aBox -> makeCopies aBox n) <$> box

{-
    ghci> box = Box "Hello"
    ghci> box
    Box "Hello"

    ghci> morePresents box 5
    Box ["Hello","Hello","Hello","Hello","Hello"]
-}


{-
QC27.2 Now suppose you have a simple box like this:

    myBox :: Box Int
    myBox = Box 1

Use fmap to put the value in your Box in another Box. Then define a function unwrap that
takes a value out of a box, and use fmap on that function to get your original box. Here’s
how your code should work in GHCi:

    GHCi> wrapped = fmap ? myBox
    GHCi> wrapped
    Box (Box 1)
    GHCi> fmap unwrap wrapped
    Box 1
-}

myBox :: Box Int
myBox = Box 1

wrapped :: Box (Box Int)
wrapped = fmap Box myBox

unwrap :: Box a -> a
unwrap (Box val) = val

unwrapped :: Box Int
unwrapped = fmap unwrap wrapped


{-
Q27.3 Write a command-line interface for partsDB that lets the user look up the cost of
an item, given an ID. Use the Maybe type to handle the case of the user entering missing
input.

Note: I'll implement this in the Main.hs file...
-}