-- Helper function, eradicates the occurence of an element e from a list
erad :: (Eq a) => a -> [a] -> [a]
erad _ [] = []
erad e (x:xs) = if (e == x)
                then erad e xs
                else x:(erad e xs)


-- Returns the number of times an element occurs consecutively.
cntr :: (Eq a) => [a] -> [(a,Int)]
cntr [] = []
cntr (x:xs) = do
              let consec lst ele
                      | null lst          = 0
                      | ele == (head lst) = 1 + consec (tail lst) ele
                      | otherwise         = 0
              let ct = (consec xs x)
              (x, ct+1):(cntr (drop ct xs))

-- Returns the elements of the list that are non-repeating
nr :: (Eq a) => [a] -> [a]
nr [] = []
nr lst = if elem (head lst) (tail lst)
         then nr (erad (head lst) (tail lst))
         else (head lst):(nr (tail lst))


-- Returns only the repeating elements of the list
ocr :: (Eq a) => [a] -> [a]
ocr [] = []
ocr (x:xs) = if elem x xs
         then x:(ocr (erad x xs))
         else ocr xs

-- Returns a list after deleting consecutively occuring elements
sasad :: (Eq a) => [a] -> [a]
sasad [] = []
sasad (x:xs) = do
              let conErad e lst
                      | null lst        = []
                      | (head lst) == e =  conErad e (tail lst)
                      | otherwise       = lst
              x:(sasad (conErad x xs))
