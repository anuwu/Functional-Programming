dividesEvenly x y = (y `div` x) * x == y
data Complex = Complex Double Double

instance Show Complex where
	show (Complex a b) = show a ++ " + " ++ show b ++ "i"

instance Eq Complex where
	(==) (Complex x1 y1) (Complex x2 y2) = (x1 == x2) && (y1 == y2)

instance Ord Complex where
	(>) (Complex x1 y1) (Complex x2 y2) = absVal (Complex x1 y1) > absVal (Complex x2 y2)
	(<=) (Complex x1 y1) (Complex x2 y2) = not ((Complex x1 y1) > (Complex x2 y2))
	(<) (Complex x1 y1) (Complex x2 y2) = absVal (Complex x1 y1) < absVal (Complex x2 y2)
	(>=) (Complex x1 y1) (Complex x2 y2) = not ((Complex x1 y1) < (Complex x2 y2))

absVal :: Complex -> Double
absVal (Complex a b) = sqrt (a^2 + b^2)

(|==|) :: Complex -> Complex -> Bool
(|==|) (Complex x1 y1) (Complex x2 y2) = absVal (Complex x1 y1) == absVal (Complex x2 y2)

-----------------------------------------------------------------------------------------------------

data QuadEq = QuadEq Double Double Double

instance Show QuadEq where
	show (QuadEq a b c) = show a ++ "x^2 " ++ show b ++ "x " ++ show c

instance Eq QuadEq where
	(==) (QuadEq a1 b1 c1) (QuadEq a2 b2 c2) = (a1 == a2) && (b1 == b2) && (c1 == c2)

{-|
solveQuad :: QuadEq -> (Complex, Complex)
solveQuad (QuadEq a b c) = let disc = b^2 - 4*a*c
								t1 = (-b/(2*a)
								t2 = sqrt(abs(disc))/(2*a) in (if (disc >= 0) then (Complex 0 0, Comple 0 0) else (Complex 0 0, Complex 0 0))
-}

{-|
solveQuad :: (Num a) => (a, a, a) -> Complex
solveQuad (a, b, c)
	| disc >= 0 = Complex ((-b + sqrt(disc))/(2*a) 0)
	| otherwise = Complex ((-b/(2*a)) (sqrt(-disc))/(2*a))
	where disc = b^2 - 4*a*c
-}

isFour :: Int -> Bool
isFour x = let four = 4 in (x == four)