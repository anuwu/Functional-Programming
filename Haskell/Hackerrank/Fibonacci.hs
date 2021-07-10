fib :: Int -> Int
fib 1 = 0
fib 2 = 1 
fib n = do
        let fibh a b n
                | n == 2 = b
                | otherwise = fibh b (a+b) (n-1)
        fibh 0 1 n
