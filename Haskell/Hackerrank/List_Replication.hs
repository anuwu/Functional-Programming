f :: [Int] -> [Int]
f lst = do
    let l = length (lst)
    case l of 0 -> []
              1 -> []
              2 -> [lst !! 1]
              3 -> [lst !! 1]
              l -> [lst !! x | x <- [1, 3 .. l-1]]