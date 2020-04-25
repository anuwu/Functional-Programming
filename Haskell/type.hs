dividesEvenly x y = (y `div` x) * x == y
data Complex = Complex Double Double

instance Show Complex where
	show (Complex a b) = show a ++ " + " ++ show b ++ "i"

instance Eq Complex where
	(==) (Complex x1 y1) (Complex x2 y2) = (x1 == x2) && (y1 == y2)

absVal :: Complex -> Double
absVal (Complex a b) = sqrt (a^2 + b^2)

(|==|) :: Complex -> Complex -> Bool
(|==|) (Complex x1 y1) (Complex x2 y2) = absVal (Complex x1 y1) == absVal (Complex x2 y2)