-- 6.1
homemadeRepeat x = cycle [x]
-- 6.2
subseq x y list = take diff (drop x list)
                    where diff = y - x
-- 6.3
inFirstHalf x list = elem val firstHalf
                        where midpoint = (length myList) 'div' 2
                              firstHalf = take midpoint list