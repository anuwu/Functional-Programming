{-
This is parsed as ([10..20] !! 3) + 50 = 13 + 50 = 63
because !! has higher precedence
instead of
[10..20] !! (3 + 50) = 10 !! 53 --> error
-}
prec9_1 = [10..20] !! 3 + 5

{-
Evaluates as ((4-2)+10)*5 = 60
instead of
4*5 + 10 - 2 = 28
-}
prec9_2 = (mul . add . sub) 4
    where
        sub = (+(-2))
        add = (+10)
        mul = (*5)