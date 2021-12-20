main = do
    _ <- getLine
    line <- getLine
    let nums = map (\x -> read x :: Int) $ words line
    print $ foldl1 lcm nums

