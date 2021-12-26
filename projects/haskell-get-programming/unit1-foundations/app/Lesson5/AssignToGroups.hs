assignToGroups :: (Num a, Enum a) => a -> [b] -> [(a, b)]
assignToGroups n aList = zip groups aList
  where
    groups = cycle [1 .. n]

repeatForever val = cycle [val]

subseq startPos endPos aList = take seqLength (drop startPos aList)
  where
    seqLength = endPos - startPos

inFirstHalf val aList = val `elem` firstHalf
  where
    midPoint = length aList `div` 2
    firstHalf = take midPoint aList

main :: IO ()
main = do
  print (take 8 teamList)
  where
    employees = [1 .. 10000]
    teamList = assignToGroups 5 employees
