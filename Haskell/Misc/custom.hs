myinit :: [a] -> [a]
myinit lst = if null (tail lst) then [] else (head lst) : myinit (tail lst)

mylast :: [a] -> a
mylast lst = if null (tail lst) then head lst else mylast (tail lst)

myapplst :: [a] -> [a] -> [a]
myapplst [] l2 = l2
myapplst l1 l2 = myapplst (init l1) (last l1 : l2)

myapp :: [a] -> a -> [a]
myapp lst x = if null lst then x:[] else (head lst) : (myapp (tail lst) x)

-- Is Int the most appropriate type?
mylen :: [a] -> Int
mylen [] = 0
mylen lst = 1 + mylen (tail lst)

-- Is Int the most appropriate type?
myind :: [a] -> Int -> a
myind lst 0 = head lst
myind lst i = myind (tail lst) (i - 1)

-- Eq typeclass
myelem :: (Eq a) => [a] -> a -> Bool
myelem [] x = False
myelem lst x = if ((head lst) == x) then True else myelem (tail lst) x

myrev :: [a] -> [a]
myrev [] = []
myrev lst = last lst : myrev (init lst)

-- Ord typeclass
mymax :: (Ord a) => a -> a -> a
mymax x1 x2 = if x1 > x2 then x1 else x2
mymin :: (Ord a) => a -> a -> a
mymin x1 x2 = if x1 < x2 then x1 else x2

-- Ord typeclass
mymaximum :: (Ord a) => [a] -> a
mymaximum lst = if null (tail lst) then head lst
                                   else mymax (head lst) (mymaximum (tail lst))
myminimum :: (Ord a) => [a] -> a
myminimum lst = if null (tail lst) then head lst
                                   else mymin (head lst) (myminimum (tail lst))

linspace :: Double -> Double -> Double -> [Double]
linspace a b h = if (a > b) then [] else a:(linspace (a+h) b h)