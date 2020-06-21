myLength xs = sum [1 | _ <- xs]
stringListHeads xs = [head x | x <- xs]

let rectangles = [ (a,b) | a <- [1 .. 10], b <- [1 .. 10]]
let specialRects = [r | r <- rectangles, fst r + snd r == (fst r) * (snd r)]