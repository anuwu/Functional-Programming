largestDiv :: Int -> Int
largestDiv n = if n <= 1
               then n
               else do
                    let divSrch n m
                            | (m == 1) = 1
                            | (n `mod` m == 0) = m
                            | otherwise = divSrch n (m-1)
                    divSrch n (n-1)

isPrime :: Int -> Bool
isPrime n = (largestDiv n) == 1

intLog :: Int -> Int -> Int
intLog n k
    | (k <= 1) = 0
    | (n == 0) = (-1)
    | otherwise = 1 + intLog (div n k) k

revNum :: Int -> Int
revNum n = do
           let sgn = if n > 0 then False else True
           let revHelp n r
                   | (n == 0) = r
                   | otherwise = revHelp (div n 10) (r*10 + mod n 10)
           let revd = revHelp (abs n) 0
           if sgn then (-revd) else revd