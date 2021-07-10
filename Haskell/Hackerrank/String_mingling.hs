main = do
    str1 <- getLine
    str2 <- getLine
    putStrLn $ concat $ map (\(ch1, ch2) -> [ch1, ch2]) $ zip str1 str2
