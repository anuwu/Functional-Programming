mylength :: [a] -> Int
mylength lst = foldr (\_ acc -> acc + 1) 0 lst

appendright :: [a] -> a -> [a]
appendright xs x = foldr (\x acc -> x:acc) [x] xs

myconcat :: [[a]] -> [a]
myconcat lst = foldr (++) [] lst

mymaximum :: (Ord a) => [a] -> a
mymaximum lst = foldl1 max lst

lstToNum :: [Integer] -> Integer
lstToNum lst = foldl (\acc x -> 10*acc + x) 0 lst