import Data.List
import System.IO

-- Hello World function
main = do
       putStrLn "What's your name?"
       name <- getLine
       putStrLn ("Hello " ++ name)
