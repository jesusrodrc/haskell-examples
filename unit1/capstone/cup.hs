cup flOz = \message -> message flOz
getOz aCup = aCup (\variable -> variable)
drink aCup ozDrank = if ozDiff > 0
                     then cup ozDiff
                     else cup 0
    where flOz = getOz aCup
          ozDiff = flOz - ozDrank

isEmpty aCup = getOz aCup == 0