sumPow_aux :: Int -> [Int] -> Int -> Int
sumPow_aux acc lst tot = 
    if acc > tot
        then 0 
    else (
        if acc == tot then 1
        else 
            case lst of
                [] -> 0
                (x:xs) ->
                    let with = sumPow_aux (acc + x) xs tot in
                    let without = sumPow_aux acc xs tot in
                    with + without
    )

root :: Int -> Int -> Int
root x 1 = x
root x n = 
    let exp = 1/(fromIntegral n) in
    let frac = (fromIntegral x)**exp in
    ceiling frac


sumPow :: Int -> Int -> Int
sumPow x n = sumPow_aux 0 (map (\e -> e^n) [1 .. root x n]) x

main = do
    x_str <- getLine
    let x = read x_str :: Int
    n_str <- getLine
    let n = read n_str :: Int
    let ans = sumPow x n
    putStrLn $ show ans