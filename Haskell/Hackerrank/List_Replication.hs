f :: Int -> [Int] -> [Int]
f n arr = concat [replicate n x | x <- arr]

