reduce [] = []
reduce [a] = [a]
reduce (ch:str) =
    let rem = reduce str
    in if elem ch str then rem else ch:rem 

main = do
    str <- getLine
    putStrLn $ reverse $ reduce $ reverse str
