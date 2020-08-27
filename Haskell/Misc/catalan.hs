catalan :: Int -> [Integer]
catalan 0 = 1:[]
catalan n = sum (zipWith (*) cat (reverse cat)) : cat
            where cat = catalan (n-1)

catalanNum :: Int -> Integer
catalanNum n = head (catalan n)