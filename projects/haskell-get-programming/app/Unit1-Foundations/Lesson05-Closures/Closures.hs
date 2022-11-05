inc :: Num a => a -> a
inc x = x + 1

ifEven :: Integral p => (p -> p) -> p -> p
ifEven func x = if even x then func x else x

genIfEven :: Integral p => (p -> p) -> p -> p
genIfEven f = (\x -> ifEven f x)

ifEvenInc :: Integer -> Integer
ifEvenInc = genIfEven inc

genIfXEven :: Integral p => p -> (p -> p) -> p
genIfXEven x = (\f -> ifEven f x)

if2Even :: (Integer -> Integer) -> Integer
if2Even = genIfXEven 2

main :: IO ()
main = do
  print result
  where
    result = if2Even inc
