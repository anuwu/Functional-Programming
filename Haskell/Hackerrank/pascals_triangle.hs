const_pascal [a] = []
const_pascal (x1:x2:xs) = let rst = const_pascal (x2:xs) in
    (x1+x2):rst

print_pascal :: Int -> IO [Int]
print_pascal 1 = do
    print_lst [1]
    return [1]

print_pascal n = do
    lst <- print_pascal $ n - 1
    let lst' = [1] ++ (const_pascal lst) ++ [1]
    print_lst lst'
    return lst'

print_lst [] = return ()
print_lst [a] = putStrLn $ show a
print_lst (x:xs) = do
    putStr $ (show x ++ " ")
    print_lst xs

main = do
    n_str <- getLine
    let n = (read n_str :: Int)
    _ <- print_pascal n
    return ()
