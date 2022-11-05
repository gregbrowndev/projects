import Data.List (sort, sortBy)

names :: [([Char], [Char])]
names =
  [ ("Ian", "Curtis"),
    ("Bernard", "Sumner"),
    ("Peter", "Hook"),
    ("Stephen", "Morris"),
    ("Bill", "Morris")
  ]

compareLastNames :: (Ord b, Ord a) => (a, b) -> (a, b) -> Ordering
compareLastNames name1 name2
  | lastName1 > lastName2 = GT
  | lastName1 < lastName2 = LT
  | firstName1 > firstName2 = GT
  | firstName1 < firstName2 = LT
  | otherwise = EQ
  where
    firstName1 = fst name1
    lastName1 = snd name1
    firstName2 = fst name2
    lastName2 = snd name2

compareLastNames2 :: (Ord a1, Ord a2) => (a1, a2) -> (a1, a2) -> Ordering
compareLastNames2 name1 name2 =
  if lastNameOrdering /= EQ
    then lastNameOrdering
    else compare (fst name1) (fst name2)
  where
    lastNameOrdering = compare (snd name1) (snd name2)

main :: IO ()
main = do
  print "Sorting a list of tuples"
  print "Using sort"
  print sortedNames
  print "Using sortBy with a ordering function"
  print sortedNames2
  where
    sortedNames = sort names
    sortedNames2 = sortBy compareLastNames2 names
