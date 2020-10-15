-- Kadane's algorithm - Find the maximum subarray sum

-- Helper function for kadane's algorithm
kad :: [Int] -> [Int]
kad (x:xs)
    | null xs       = [x]
    | otherwise     = (x + max (head rst) 0):rst
    where rst = kad xs

kadane :: [Int] -> Int
kadane [] = 0
kadane lst = maximum (kad lst)

-- Finds the maximum of the scanl of the accumulator function
kadane1 :: [Int] -> Int
kadane1 [] = 0
kadane1 (x:xs) = maximum $ scanl (\acc x -> x + if acc < 0 then 0 else acc) x xs
