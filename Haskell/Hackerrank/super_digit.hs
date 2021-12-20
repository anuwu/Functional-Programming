split :: String -> String
split (ch:str) = if ch == ' ' then str else split str

string_sum [] = 0
string_sum (ch:str) = (+) (read [ch] :: Int) $ string_sum str

bread :: Int -> [Int]
bread num = bread' num []
        where bread' n lst
                    | n == 0        = lst
                    | otherwise     = bread' (div n 10) ((mod n 10):lst)

super_digit n =
    let broken = bread n
        len = length broken
    in if len == 1
    then n
    else super_digit $ sum broken

main = do
    str <- getLine
    let n_str = split $ reverse str
    let k = read (split str) :: Int
    print $ super_digit ((*) k $ string_sum n_str)
