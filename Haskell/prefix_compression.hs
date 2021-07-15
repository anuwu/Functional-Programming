get_preflen str1 [] = 0
get_preflen [] str2 = 0
get_preflen (c1:s1) (c2:s2) = 
    if (c1 == c2) then (1 + get_preflen s1 s2) else 0

main = do
    str1 <- getLine
    str2 <- getLine
    let len = get_preflen str1 str2
    let pref = take len str1
    let out_string str = (show $ length str) ++ " " ++ str
    mapM putStrLn $ map (\str -> (show $ length str) ++ " " ++ str) [pref, drop len str1, drop len str2] 
