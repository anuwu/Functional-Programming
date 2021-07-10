import qualified Data.Map as Map
import Control.Monad

tupstr_to_tuplst :: [String] -> [(Int, Int)]
tupstr_to_tuplst = map (\ln ->
    let dl = map (\x -> read x :: Int) $ words ln
    in (head dl, dl !! 1)
    )

val_tups_aux [] mp = True
val_tups_aux (hd:tl) mp = let mem = Map.member inp mp in
    if mem then (
        let (Just x) = Map.lookup inp mp in
        x == out
    )
    else val_tups_aux tl $ Map.insert inp out mp
    where (inp, out) = hd

val_tups :: [(Int, Int)] -> Bool
val_tups lst = val_tups_aux lst Map.empty

check_func = do
    no_tups_str <- getLine
    let no_tups = read no_tups_str :: Int
    tupStr <- sequence $ replicate no_tups getLine
    return $ val_tups $ tupstr_to_tuplst tupStr

main = do
    no_tc_str <- getLine
    let no_tc = read no_tc_str :: Int
    isfuncs <- sequence $ replicate no_tc check_func
    forM isfuncs (\b -> do
        putStrLn (if b then "YES" else "NO")
        )

