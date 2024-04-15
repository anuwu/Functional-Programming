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
    | len == 0      = ([], [])
    | ind <= 0      = ([], lst)
    | otherwise     = ((head lst):l1, l2)
    where
        (l1, l2) = mySplitAt (ind - 1) (tail lst)
        len = length lst
