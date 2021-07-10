import Data.Char

main = do
    putStr "Please enter your name : "
    name <- getLine
    putStrLn $ "Hello " ++ name
    let cap = map toUpper name in do
        putStrLn $ "Your name capitalized is " ++ cap
    return ()
