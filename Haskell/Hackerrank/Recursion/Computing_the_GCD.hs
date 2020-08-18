gcd' :: Integral a => a -> a -> a
gcd' 0 m = m
gcd' n m = gcd (m `mod` n) n
