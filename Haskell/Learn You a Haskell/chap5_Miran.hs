qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort bigger
               where smaller = [a | a <- xs, a <= x]
                     bigger = [a | a <- xs, a > x]