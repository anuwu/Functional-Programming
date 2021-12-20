doubleMe x = x + x
doubleUs x y = x + x + y + y
doubleSmallNumber x = if x > 100 then x else x*2

myLength xs = sum [1 | _ <- xs]
stringListHeads xs = [head x | x <- xs]

rectangles = [ (a,b) | a <- [1 .. 10], b <- [1 .. 10]]
specialRects = [r | r <- rectangles, fst r + snd r == (fst r) * (snd r)]
