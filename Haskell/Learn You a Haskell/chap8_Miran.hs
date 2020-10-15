import qualified Data.Map as Map

------------------------ Complex Numbers  ------------------------ 

-- Complex number with real and imaginary part
data Complex a = Complex {real :: a, im :: a}  

-- Printing a complex number
instance Show t => Show (Complex t) where
        show (Complex rl im) = show rl ++ " + " ++ show im ++ "i"

{-
modulus :: (Num t, Floating a) => Complex t -> a
Does not work
-}
modulus :: (Floating t) => Complex t -> t
modulus (Complex rl im) =  sqrt $ rl**2 + im**2

------------------------ Locker Map ------------------------ 

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
        case Map.lookup lockerNumber map of
                Nothing -> Left $ "Locker Number" ++ show lockerNumber ++ " doesn't exist!"
                Just (state, code) -> if state /= Taken
                                        then Right code
                                        else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

-- Need this to correctly infer the type
-- A number like '100' is taken as Integer instead of Int
lockers :: LockerMap
lockers = Map.fromList [
        (100, (Taken, "ZD39I")),
        (101, (Free, "JAH3I")),
        (103, (Free, "IQSA9")),
        (105, (Free, "QOTSA")),
        (109, (Taken, "893JJ")),
        (110, (Taken, "99292"))
        ]
 
------------------------ LinkedList ------------------------ 

-- Cons operator
infixr 5 :-: 

-- A list is either empty | consists of a head followed by a tail
data LinkedList a = Empty | a :-: (LinkedList a) deriving (Read, Eq, Ord)

-- Printing of a linked list
instance Show t => Show (LinkedList t) where
        show Empty = "_"
        show (hd :-: tl) = show hd ++ " -> " ++ show tl

-- Appending two linked lists
infixr 5 .++
(.++) :: LinkedList a -> LinkedList a -> LinkedList a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

-- Element wise addition of two linked lists
infixl 6 .+
(.+) :: (Num a) => LinkedList a -> LinkedList a -> Maybe (LinkedList a)
Empty .+ Empty = Just Empty
Empty .+ ys = Nothing
xs .+ Empty = Nothing
(x :-: xs) .+ (y :-: ys) = let tls = xs .+ ys in
                        case tls of
                        Nothing -> Nothing
                        Just tl -> Just $ (x+y) :-: tl
                        
{-
The definition below does not work -

(:+:) :: (Num a) => LinkedList a -> LinkedList a -> Maybe (LinkedList a)
Empty :+: ys = Nothing
xs :+: Empty = Nothing
(x :-: xs) :+: (y :-: ys) = let tls = xs :+: ys in
                        case tls of
                        Nothing -> Nothing
                        Just tls -> Just $ (x+y) :-: tls 
-}

------------------------ Binary Search Tree ------------------------ 

-- A tree is either empty | consists of a nood with two subtrees
data Tree a = NoRoot | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

{- 
Constant for defining empty and tree
Although this seems like a meaningful constant with which to rename 'NoRoot', 
it causes the Haskell compiler to give warnings
-}
emptyTree :: Tree a
emptyTree = NoRoot

-- A tree with one element
root :: (Ord a) => a -> Tree a
root x = Node x NoRoot NoRoot 

-- Tree insertion procedure
treeIns :: (Ord a) => a -> Tree a -> Tree a
treeIns x NoRoot = root x
treeIns x (Node n left right)
        | x < n         = Node n (treeIns x left) right
        | x > n         = Node n left (treeIns x right)
        | otherwise     = Node n left right

-- Checks if an element is part of a tree
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x NoRoot = False
treeElem x (Node n l r)
        | x < n         = treeElem x l
        | x > n         = treeElem x r
        | otherwise     = True
