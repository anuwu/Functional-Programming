func :: Double -> [Int] -> [Int] -> Double
func _ [] [] = 0.0
func x a b = (fromIntegral (head a)) * (x**(fromIntegral (head b))) + (func x (tail a) (tail b))

linspace :: Double -> Double -> Double -> [Double]
linspace r l h = if (r < l) then [] else r:(linspace (r-h) l h)

area :: [Double] -> [Int] -> [Int] -> Double
area [] _ _ = 0
area xs a b = (func (head xs) a b) * 0.001 + area (tail xs) a b 

volume :: [Double] -> [Int] -> [Int] -> Double
volume [] _ _ = 0
volume xs a b = ((func (head xs) a b)^2) * 0.001 + volume (tail xs) a b

solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b = do
                let xs = linspace (fromIntegral r) (fromIntegral l) 0.001
                [area xs a b, pi * volume xs a b]
