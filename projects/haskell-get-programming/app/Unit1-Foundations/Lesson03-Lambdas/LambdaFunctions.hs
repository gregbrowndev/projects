doubleDouble :: Num a => a -> a
doubleDouble x = dubs * 2
  where
    dubs = x * 2

doubleDouble2 :: Num a => a -> a
doubleDouble2 x = (\x -> x * 2) x * 2

doubleDouble3 :: Num a => a -> a
doubleDouble3 x =
  let dubs = (x * 2)
   in dubs * 2

overwrite :: Num p1 => p2 -> p1
overwrite x =
  let x = 2
   in let x = 3
       in let x = 4
           in x

counter :: Num a => a -> a
counter x = (\x -> x + 1) ((\x -> x + 1) ((\x -> x) x))
