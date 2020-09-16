import qualified Data.Char as Char

myElem e xs = (length filteredList) /= 0
    where filteredList = filter (== e) xs

betterIsPalindrome text = transformedPhrase == transformedPhrase
    where filteredPhrase = filter (/= ' ') text 
          transformedPhrase = map Char.toLower filteredPhrase
                  

harmonic n = sum (take n seriesValues)
    where seriesPairs = zip (cycle [1.0])  [1.0,2.0 .. ]        
          seriesValues = map                       
                        (\pair -> (fst pair)/(snd pair))
                        seriesPairs