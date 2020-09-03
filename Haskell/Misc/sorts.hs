qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort (x:xs) = (qsort less) ++ [x] ++ (qsort more)
               where less = filter (<=x) xs
                     more = filter (>x) xs

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x<y
                then x : (merge xs (y:ys))
                else y : (merge (x:xs) ys)

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort lst = let left = take haf lst ; 
                    right = drop haf lst
                in merge (mergesort left) (mergesort right)
                where haf = div (length lst) 2 

isort :: (Ord a) => [a] -> [a]
isort [] = []
isort [x] = [x]
isort (x:xs) = ins x (isort xs)
        where ins x lst
                | null lst              = [x]
                | x > (head lst)        = (head lst) : (ins x (tail lst))
                | otherwise             = x:lst
