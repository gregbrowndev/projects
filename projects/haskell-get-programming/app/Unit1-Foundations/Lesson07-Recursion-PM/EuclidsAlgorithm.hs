myGCD :: Integral t => t -> t -> t
myGCD a b =
  if remainder == 0 then b else myGCD b remainder
  where
    remainder = a `mod` b

myGCD2 :: Integral t => t -> t -> t
myGCD2 a 0 = a
myGCD2 a b = myGCD2 b (a `mod` b)

main :: IO ()
main = do
  print result
  where
    result = myGCD2 15 100
