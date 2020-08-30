applyN :: (a -> a) -> Int -> a -> a
applyN f n x
    | n == 1       = f x
    | otherwise    = f (applyN f (n-1) x)

myflip1 :: (a -> b -> c) -> (b -> a -> c)
myflip1 f = g
    where g x y = f y x

myflip2 :: (a -> b -> c) -> b -> a -> c
myflip2 f x y = f y x

myflip3 :: (a -> b -> c) -> b -> a -> c
myflip3 f = \x y -> f y x

myelem :: (Eq a) => a -> [a] -> Bool
myelem x xs = foldl (\acc y -> if x == y then True else acc) False xs

mymap :: (a -> b) -> [a] -> [b]
mymap f lst = foldr (\x acc -> (f x) : acc) [] lst

myrev1 :: [a] -> [a]
myrev1 lst = foldl (\acc x -> x:acc) [] lst

myrev2 :: [a] -> [a]
myrev2 lst = foldl (flip (:)) [] lst

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter f lst = foldr (\x acc -> if f x then x:acc else acc) [] lst

myhead :: [a] -> a
myhead lst = foldr1 (\x _ -> x) lst

mylast :: [a] -> a
mylast lst = foldl1 (\_ x -> x) lst