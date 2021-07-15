import Data.List

compress [] = []
compress msg = concat $ map (\(ch:rst) -> if rst==[] then [ch] else [ch] ++ (show $ length rst + 1)) (group msg)

main = do
    msg <- getLine
    putStrLn $ compress msg 
