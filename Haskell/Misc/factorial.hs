import Control.Monad.Fix

g :: (Integer -> Integer) -> (Integer -> Integer)
g x = (\n -> if n == 0 then 1 else n * x (n - 1))

x0 :: Integer -> Integer
x0 = undefined

fact = fix (\rec n -> if n == 0 then 1 else n * rec (n-1))
{-
fix (\rec n -> if n == 0 then 1 else n * rec (n-1)) 5
(let x = (\rec n -> if n == 0 then 1 else n * rec (n-1)) x in x) 5										--> Step (0)

What the following link (https://en.wikibooks.org/wiki/Haskell/Fix_and_recursion) says to do this		--> Step (n)
(let x n = if n == 0 then 1 else n * x (n-1) in x) 5

Typing the above in Ghci gives 120, and this is possible if the expression can be parsed as 			--> Step (1)
let x n = if n == 0 then 1 else n * x (n-1) in x 5

Typing the above in Ghci gives 120, and this is possible if												--> Step (2)
let x = (\rec n -> if n == 0 then 1 else n * rec (n-1)) x in x 5

But, following step (2), shouldn't the next step after step (0) be... continue tomorrow
-}

