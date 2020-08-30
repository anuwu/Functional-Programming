primes :: Int -> [Int]
primes n = sieve [2..n]
        where sieve lst
                | null lst      = []
                | otherwise     = x:(sieve [y | y <- xs, mod y x > 0])
                where x = head lst
                      xs = tail lst