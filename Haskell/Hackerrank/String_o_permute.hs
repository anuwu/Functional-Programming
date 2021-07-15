main = do
    num_tc_str <- getLine
    let num_tc = read num_tc_str :: Int
    inps <- sequence $ replicate num_tc getLine
    let outs = map string_permute inps
    mapM putStrLn outs
    
string_permute :: String -> String
string_permute [] = []
string_permute (c1:c2:str) = c2:c1:(string_permute str)
