len :: [a] -> Int
len lst = case lst of [] -> 0
                      lst -> 1 + len (tail lst)
