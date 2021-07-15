import Control.Monad


shift_right [] = []
shift_right [a] = [a]
shift_right (ch:str) = str ++ [ch]

rotate str = 
    let rotate' len lst = if len==1
                        then lst
                        else rotate' (len-1) (shift_right (head lst) : lst)
    in map reverse $ rotate' (length str) [reverse str] 

main = do
    tc_str <- getLine
    let no_tc = read tc_str :: Int
    inps <- sequence $ replicate no_tc getLine
    let ans = map rotate inps
    forM ans (\x -> print_rots x)
    where print_rots lst = case lst of
                    (s:[])      -> putStrLn s
                    (s:lst')    -> do
                                putStr $ s ++ " "
                                print_rots lst'

