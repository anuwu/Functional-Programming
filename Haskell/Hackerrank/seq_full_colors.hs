full_color str =
    let check_pref c1 c2 n1 n2 str = case str of
                                        []      -> n1 == n2
                                        (ch:st) -> check_ch ch 
                                            where   diff_check x y = abs (x - y) <= 1 
                                                    check_ch c
                                                        | c == c1       = if diff_check (n1 + 1) n2
                                                                        then check_pref c1 c2 (n1+1) n2 st
                                                                        else False
                                                        | c == c2       = if diff_check n1 (n2 + 1)
                                                                        then check_pref c1 c2 n1 (n2+1) st
                                                                        else False
                                                        | otherwise     = check_pref c1 c2 n1 n2 st
    in (check_pref 'R' 'G' 0 0 str) && (check_pref 'Y' 'B' 0 0 str)

main = do
    tc_str <- getLine
    let no_tc = read tc_str :: Int
    inp_lst <- sequence $ replicate no_tc getLine
    mapM print $ map full_color inp_lst
