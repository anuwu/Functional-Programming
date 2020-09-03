-- Kadane's algorithm - Find the maximum subarray sum

kad :: [Int] -> [Int]
kad (x:xs)
    | null xs       = [x]
    | otherwise     = (x + max (head rst) 0):rst
    where rst = kad xs

kadane :: [Int] -> Int
kadane [] = 0
kadane lst = maximum (kad lst)


kadane1 :: [Int] -> Int
kadane1 [] = 0
kadane1 (x:xs) = maximum $ scanl (\acc x -> x + if acc < 0 then 0 else acc) x xs

{-
 - kadane [1,2,3,-4,10,-60]
 -  12
 -
 -
 -
 -
 -
 -
 -
 - -}

