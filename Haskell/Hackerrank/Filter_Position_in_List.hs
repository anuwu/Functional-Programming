f :: [Int] -> [Int]
f lst = [lst !! x | x <- [1, 3 .. (length lst)-1]]
