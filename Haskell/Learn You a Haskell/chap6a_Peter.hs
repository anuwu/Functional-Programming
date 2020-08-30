firstThat :: (a -> Bool) -> a -> [a] -> a
firstThat f = foldr (\x acc -> if f x then x else acc)

lastThat :: (a -> Bool) -> a -> [a] -> a
lastThat f = foldl (\acc x -> if f x then x else acc)

argmax :: (Ord b) => (a -> b) -> [a] -> a
argmax f = foldl1 (\acc x -> if f x > f acc then x else acc)