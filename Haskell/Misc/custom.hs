mylast :: [a] -> a
mylast (l:[]) = l
mylast (l:lst) = mylast lst
mylast [] = error "custom : Empty List"

myinit :: [a] -> [a]
myinit (l:lst) = if null lst then [] else l : (myinit lst)
myinit [] = error "custom : Empty list"

myapplst :: [a] -> [a] -> [a]
myapplst [] l2 = l2
myapplst l1 l2 = myapplst (init l1) (last l1 : l2)

myapp :: [a] -> a -> [a]
myapp [] x = [x]
myapp lst x = (head lst) : (myapp (tail lst) x)

-- Is Int the most appropriate type?
mylen :: [a] -> Int
mylen [] = 0
mylen lst = 1 + mylen (tail lst)

-- Is Int the most appropriate type?
myind :: [a] -> Int -> a
myind [] _ = error "custom : Index out of bounds"
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
mymaximum [] = error "custom : Empty List"
mymaximum (l:lst) = if null lst then l else mymax l (mymaximum lst) 
                   
myminimum :: (Ord a) => [a] -> a
myminimum [] = error "custom : Empty List"
myminimum (l:lst) = if null lst then l else mymin l (myminimum lst) 

linspace :: Double -> Double -> Double -> [Double]
linspace a b h = if (a > b) then [] else a:(linspace (a+h) b h)

mytake :: [a] -> Int -> [a]
mytake _ 0 = []
mytake [] _ = []
mytake lst i = if null lst then error "custom : Empty List" else (head lst):(mytake (tail lst) (i-1)) 

mydrop :: [a] -> Int -> [a]
mydrop [] _ = []
mydrop lst 0 = lst
mydrop lst i = mydrop (tail lst) (i-1)

myzip :: [a] -> [b] -> [(a,b)]
myzip [] _ = []
myzip _ [] = []
myzip l1 l2 = (head l1, head l2):(myzip (tail l1) (tail l2)) 

mymap :: [a] -> (a -> b) -> [b]
mymap [] _ = []
mymap lst f = (f (head lst)):(mymap (tail lst) f) 

myreduce :: [a] -> (a -> a -> a) -> a -> a 
myreduce [] _ seed = seed
myreduce lst f seed = f (head lst) (myreduce (tail lst) f seed) 

myremoveIf :: [a] -> (a -> Bool) -> [a]
myremoveIf [] _ = []
myremoveIf lst f = if (f (head lst)) then myremoveIf (tail lst) f else (head lst):(myremoveIf (tail lst) f)  

myreplicate :: a -> Int -> [a] 
myreplicate x 0 = []
myreplicate x i = x : myreplicate x (i-1)

pow :: Int -> Int -> Int
pow _ 0 = 1
pow x 1 = x
pow x n = if (mod n 2 == 0) then
                              (pow x (div n 2)) * (pow x (div n 2))
                              else
                              x * (pow x (n-1))

plus :: Int -> Int -> Int
plus x 0 = x
plus x y = succ (plus x (y-1))

mul :: Int -> Int -> Int
mul _ 0 = 0
mul x 1 = x
mul x y = plus x (mul x (y-1))