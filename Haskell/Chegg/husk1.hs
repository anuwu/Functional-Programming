{-
Takes an element, and a list, and removes all those elements from
the list which are not equal to the given element
-}
removeAllExcept :: (Eq a) => a -> [a] -> [a]
removeAllExcept _ [] = []
removeAllExcept e (x:xs) = let fxs = removeAllExcept e xs
                        in if (e == x) then e:fxs else fxs

{-
Takes an element, and a list, and removes all those elements from
the list which are equal to the given element
-}
removeAll :: (Eq a) => a -> [a] -> [a]
removeAll _ [] = []
removeAll e (x:xs) = let fxs = removeAll e xs
                    in if (e == x) then fxs else x:fxs

{-
Replace all occurrences of the first argument with the second argument 
in the list and return the new list
-}
substitute :: (Eq a) => a -> a -> [a] -> [a]
substitute _ _ [] = []
substitute a b (x:xs) = let fxs = substitute a b xs
                        in if (a == x) then b:fxs else x:fxs


-- Merges two sorted lists into one
mergeSorted2 :: (Ord a) => [a] -> [a] -> [a]
mergeSorted2 [] l2 = l2
mergeSorted2 l1 [] = l1
mergeSorted2 (x1:x1s) (x2:x2s) = let
                                h1 = mergeSorted2 x1s (x2:x2s)
                                h2 = mergeSorted2 (x1:x1s) x2s
                                in if (x1 < x2) then x1:h1 else x2:h2

-- Merges 3 sorted lists into one. Uses mergeSorted2
mergeSorted3 :: (Ord a) => [a] -> [a] -> [a] -> [a]
mergeSorted3 l1 l2 l3 = let l12 = mergeSorted2 l1 l2
                        in mergeSorted2 l12 l3