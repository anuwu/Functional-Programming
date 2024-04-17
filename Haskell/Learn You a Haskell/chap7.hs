mytranspose :: [[a]] -> [[a]] 
mytranspose mat = transhelp (maximum $ map length mat) mat []
    where transhelp 0 mat lst = lst
          transhelp n mat lst = transhelp (n-1) mat ([row !! (n-1) | row <- mat, n <= length row]:lst)

myintercalate :: [a] -> [[a]] -> [a]
myintercalate l1 l2 = concat $ helper l1 l2                               
    where helper ls1 [] = [[]]                                              
          helper ls1 [x] = [x]                                              
          helper ls1 (el:ls2) = [el, ls1] ++ (helper ls1 ls2)

myintersperse :: a -> [a] -> [a]
myintersperse el [] = []
myintersperse el [x] = [x]
myintersperse el (x:xs) = let right = myintersperse el xs in (x:el:right)

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat [x] = x
myconcat (x:xs) = x ++ myconcat xs

myconcatMap :: (a -> [b]) -> [a] -> [b]
myconcatMap func = concat . map func

myAnd :: [Bool] -> Bool
myAnd = foldl (\x y -> x && y) True

myOr :: [Bool] -> Bool
myOr = foldl (\x y -> x || y) False

myAll :: (a -> Bool) -> [a] -> Bool
myAll pred = and . map pred

myAny :: (a -> Bool) -> [a] -> Bool
myAny pred = not . and . map (not . pred)

mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt ind lst
    | ind <= 0          = ([], lst)
    | [] <- lst         = ([], [])
    | (l:ls) <- lst     = let (l1, l2) = mySplitAt (ind-1) ls in (l:l1, l2)

myIterate :: (a -> a) -> a -> [a]
myIterate f v = v : (myIterate f (f v))

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f [] = []
myTakeWhile f lst
    | f l           = l : myTakeWhile f ls
    | otherwise     = []
    where
        (l:ls) = lst

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile f [] = []
myDropWhile f lst
    | f l           = myDropWhile f ls
    | otherwise     = lst
    where
        (l:ls) = lst

mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan f [] = ([], [])
mySpan f lst
    | f l           = let (fs, sn) = mySpan f ls in (l:fs, sn)
    | otherwise     = ([], lst)
    where
        (l:ls) = lst

myBreak :: (a -> Bool) -> [a] -> ([a], [a])
myBreak f = mySpan (not . f)

myGroup :: (Eq a) => [a] -> [[a]]
myGroup [] = []
myGroup [x] = [[x]]
myGroup (x:xs) = 
    let (xss, cnt) = dropWhileCnt (==x) xs in (replicate (cnt+1) x):(myGroup xss)
    where
        dropWhileCnt f [] = ([], 0)
        dropWhileCnt f lst
            | f l       = let (lst1, cnt1) = dropWhileCnt f ls in (lst1, cnt1 + 1)
            | otherwise = (lst, 0)
            where
                (l:ls) = lst