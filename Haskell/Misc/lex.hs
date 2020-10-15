{-
Inspired from Lex Fridman's post dated 1 week prior to 26th August 2020

1 × 9 + 2 = 11
12 × 9 + 3 = 111
123 × 9 + 4 = 1111
1234 × 9 + 5 = 11111
12345 × 9 + 6 = 111111
123456 × 9 + 7 = 1111111
1234567 × 9 + 8 = 11111111
12345678 × 9 + 9 = 111111111
123456789 × 9 + 10 = 1111111111
-}

-- Lex Fridman's list of numbers
lexlist = [1,2,3,4,5,6,7,8,9]

-- Converts a list of integers to a whole number
listToNum :: [Int] -> Int
listToNum [] = 0
listToNum lst = let ltn lst
                        | null lst      = 0
                        | otherwise     = x + 10*(ltn xs)
                        where (x:xs) = lst
                in ltn rev
                where rev = reverse (lst)  -- Need to reverse the list first

-- Prints one line of lex's pattern
lexLine :: Int -> IO()
lexLine x = putStrLn str
            where num = listToNum (take x lexlist)
                  str = show (num) ++ " x 9 + " ++ show (x + 1) ++ " = " ++ show (num*9 + x + 1)

-- Prints all lines of lex's pattern
lexPrint :: Int -> IO()
lexPrint n
    | n == 0        = lexLine 0
    | n < 10        = do
                    lexPrint (n-1)
                    lexLine n
    | otherwise     = putStrLn "Wrong number buddy"
