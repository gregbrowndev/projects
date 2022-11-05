myHead :: [p] -> p
myHead (x:_) = x
myHead [] = error "No head for empty list"

myTail :: [a] -> [a]
myTail (_:xs) = xs
myTail [] = error "No tail for empty list"

main :: IO ()
main = do
  print (myHead [1,2,3])
  print (myTail [1,2,3])