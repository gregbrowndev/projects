main :: IO ()
main = do
  input <- getContents
  let reversed = reverse input
  putStrLn reversed
