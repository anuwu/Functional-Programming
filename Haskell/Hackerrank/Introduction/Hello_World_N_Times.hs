hellN 0 s = return ()
hellN n s = do
    putStrLn s
    hellN (n-1) s
    

main :: IO()
main = do
    n <- readLn :: IO Int
    hellN n "Hello World"
