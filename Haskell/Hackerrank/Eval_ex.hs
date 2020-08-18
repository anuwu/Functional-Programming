fact 0 = 1
fact n = n * (fact (n - 1))

expo :: Double -> Double
expo x = 1 + sum ([(x^n)/(fromIntegral (fact n)) | n <- [1 .. 9]])
