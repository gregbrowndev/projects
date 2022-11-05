-- Drop
myDrop :: (Eq t, Num t) => t -> [a] -> [a]
myDrop 0 xs       = xs
myDrop _ []       = []
myDrop n (_ : xs) = myDrop (n -1) xs

-- Length
myLength []       = 0
myLength (_ : xs) = 1 + myLength xs

-- Take
myTake 0 _ = []
myTake _ [] = []
myTake n (x : xs) = x : rest
  where
    rest = myTake (n -1) xs

-- Cycle
--myCycle xs = xs ++ myCycle xs
myCycle (x : xs) = x : myCycle (xs ++ [x])

-- Ackerman Function
ackerman 0 n = n + 1
ackerman m 0 = ackerman (m -1) 1
ackerman m n = ackerman (m -1) (ackerman m (n -1))

-- Collatz conjecture
collatz 1 = 1
collatz n =
  if even n
    then 1 + collatz (n `div` 2)
    else 1 + collatz (n * 3 + 1)

-- Reverse
myReverse []       = []
myReverse (x : xs) = myReverse xs ++ [x]

-- Slow Fibonacci numbers (exponential complexity)
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib (n -1) + slowFib (n -2)

-- Fast Fibonacci numbers
fastFib :: (Eq t1, Num t1, Num t2) => t2 -> t2 -> t1 -> t2
fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib x y 2 = x + y
fastFib x y c = fastFib (x + y) x (c - 1)

-- wrap fastFib to hide the initial args for x and y
fib :: Integer -> Integer
fib = fastFib 1 0

main :: IO ()
main = do
  print (myDrop 0 [1, 2, 3])
  print (myDrop 10 [1])
  print (myDrop 2 [1, 2, 3, 4])
