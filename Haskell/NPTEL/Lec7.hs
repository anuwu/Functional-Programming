isAlt :: (Ord a) => [a] -> Bool
isAlt [] = True
isAlt [x] = True
isAlt lst = (isUp lst) || (isDown lst)

isUp :: (Ord a) => [a] -> Bool
isUp [] = True
isUp [x] = True
isUp (x:y:xs) = (x < y) && (isDown (y:xs))

isDown :: (Ord a) => [a] -> Bool
isDown [] = True
isDown [x] = True
isDown (x:y:xs) = (x > y) && (isUp (y:xs))