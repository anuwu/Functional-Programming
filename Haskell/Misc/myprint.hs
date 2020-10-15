-- Function to print a string containing '\n' such that
-- the new line is actually printed
myprint :: String -> IO ()
myprint x = let lins = lines x in
        linePrint lins where
        linePrint lns = case lns of
                []      -> putStr ""
                l:ls    -> do { putStrLn l ; linePrint ls ; }

