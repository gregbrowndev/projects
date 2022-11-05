-- San Francisco office address
sfOffice :: ([Char], [Char]) -> [Char]
sfOffice name =
  if lastName < "L"
    then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
    else nameText ++ " - PO Box 1000 - San Francisco, CA, 94109"
  where
    lastName = snd name
    nameText = fst name ++ " " ++ lastName

-- New York office address
nyOffice :: ([Char], [Char]) -> [Char]
nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where
    nameText = fst name ++ " " ++ snd name

-- Reno office address
renoOffice :: (a, [Char]) -> [Char]
renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where
    nameText = snd name

-- Washington DC office address
dcOffice :: ([Char], [Char]) -> [Char]
dcOffice name = nameText ++ " PO Box 1337 - Washington DC, 20001"
  where nameText = fst name ++ " " ++ snd name ++ ", Esq."

-- Dispatch function
getLocationFunction :: [Char] -> ([Char], [Char]) -> [Char]
getLocationFunction location = case location of
  "ny"   -> nyOffice
  "sf"   -> sfOffice
  "reno" -> renoOffice
  _      -> (\name -> fst name ++ " " ++ snd name)

-- Function to generate the postal address for different offices
addressLetter :: ([Char], [Char]) -> [Char] -> [Char]
addressLetter name location = locationFunction name
  where locationFunction = getLocationFunction location

main :: IO ()
main = do
  print address
  where address = addressLetter ("Greg", "Brown") "reno"
