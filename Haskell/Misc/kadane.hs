-- Kadane's algorithm - Find the maximum subarray sum

kad :: [Int] -> [Int]
kad (x:xs)
    | null xs       = [x]
    | otherwise     = (x + max (head rst) 0):rst
    where rst = kad xs

kadane :: [Int] -> Int
kadane [] = 0
kadane lst = maximum (kad lst)