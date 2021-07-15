import Control.Monad

catalan n = head $ cat n []
        where cat n lst
                    | n == 0        = [1]
                    | otherwise     = let lst' = cat (n - 1) lst
                                    in (sum $ zipWith (*) lst' $ reverse lst'):lst' 

main = do
    tc_str <- getLine
    let no_tc = read tc_str :: Int
    inps_str <- sequence $ replicate no_tc getLine
    let inps = map (\x -> read x :: Int) inps_str
    mapM (\x -> putStrLn $ show $ x `mod` 100000007) $ map catalan inps
