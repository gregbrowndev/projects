-- A cup with one property: ounces of liquid that it contains
cup :: t1 -> (t1 -> t2) -> t2
cup f10z = \message -> message f10z

getOz :: ((p -> p) -> t) -> t
getOz aCup = aCup (\f10z -> f10z)

drink aCup ozDrank = if ozDiff >= 0 then cup ozDiff else cup 0
  where
    f10z = getOz aCup
    ozDiff = f10z - ozDrank

isEmpty aCup = getOz aCup == 0
