getRequestURL :: [Char] -> [Char] -> [Char] -> [Char] -> [Char]
getRequestURL host apiKey resource resourceId =
  host ++ "/" ++ resource ++ "/" ++ resourceId ++ "?token=" ++ apiKey

-- captures host in the closure to send all requests to the same host
genHostRequestBuilder :: [Char] -> [Char] -> [Char] -> [Char] -> [Char]
genHostRequestBuilder host = (\apiKey resource resourceId -> getRequestURL host apiKey resource resourceId)

genHostRequestBuilder2 :: [Char] -> [Char] -> [Char] -> [Char] -> [Char]
genHostRequestBuilder2 host = getRequestURL host


main :: IO ()
main = do
  print result
  where
    urlBuilder = genHostRequestBuilder2 "http://example.com"
    result = urlBuilder "1234Haskell" "book" "1234"
