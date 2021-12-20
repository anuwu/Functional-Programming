pentagon :: Int -> Int
pentagon 0 = 0
pentagon 1 = 1
pentagon n =
    let p = pentagon (n-1) in
    3*n + p - 2

main = do
    tc_str <- getLine
    let no_tc = read tc_str :: Int
    inp_lst <- sequence $ replicate no_tc getLine
    mapM print $ map (\x -> pentagon (read x :: Int)) inp_lst