import Data.List

------------------------------- TreeIfication ------------------------------- 

-- Returns the head of a list as singleton list, if it exists, otherwise returns the empty list
nullHead :: [a] -> [a]
nullHead lst = case lst of
        [] -> []
        _  -> [head lst]

-- Returns the tail of a list if it exists, otherwise returns the empty list
nullTail :: [a] -> [a]
nullTail lst = case lst of
        [] -> []
        _  -> tail lst
        
-- treeIfy :: (Ord a) => [a] -> [a]
treeIfy [] = []
treeIfy lst = let (lf, rt) = splitAt (div (length lst) 2) $ sort lst
        in nullHead rt ++ treeIfy lf ++ (treeIfy $ nullTail rt)

------------------------------- BST Definition ------------------------------- 

-- Type for a binary tree. It's either empty or contains a node with left/right subtrees
data Tree a = NoRoot | Node a (Tree a) (Tree a) deriving (Read, Eq)

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

-- Function to construct a tree from a list right to left
fromListRt :: (Ord a) => [a] -> Tree a
fromListRt lst = foldr treeIns NoRoot lst

-- Function to construct a tree from a list left to right
fromListLf :: (Ord a) => [a] -> Tree a
fromListLf lst = foldl (flip treeIns) NoRoot lst


-- Takes a list and inserts it into a BST such that it is height balanced
balancedIns :: (Ord a) => [a] -> Tree a
balancedIns lst = fromListLf $ treeIfy lst

-- Finds the height of the tree
height :: Tree a -> Int
height NoRoot = 0
height (Node n left right) = 1 + max (height left) (height right)

-- Checks if an element is part of a tree
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x NoRoot = False
treeElem x (Node n l r)
        | x < n         = treeElem x l
        | x > n         = treeElem x r
        | otherwise     = True

-- Printing pre-order with indentation
instance Show a => Show (Tree a) where
        show NoRoot             = ""
        show (Node nd lf rt)    = init $ treeLines 0 (Node nd lf rt) where
                                treeLines _ NoRoot              = ""
                                treeLines s (Node n l r)        = treeLines s1 r ++
                                                                treeLines s1 l ++
                                                                replicate (4*s) ' ' ++ show n ++ "\n" where
                                                                s1 = s+1 
                                                                       

-- Function to return pre-order traversal of a tree as a list
preOrder :: (Ord a) => Tree a -> [a]
preOrder NoRoot = []
preOrder (Node n lf rt) = n:(preOrder lf) ++ preOrder rt 

-- Function to return in-order traversal of a tree as a list
inOrder :: (Ord a) => Tree a -> [a]
inOrder NoRoot = []
inOrder (Node n lf rt) = inOrder lf ++ [n] ++ inOrder rt

-- Function to return post-order traversal of a tree as a list
postOrder :: (Ord a) => Tree a -> [a]
postOrder NoRoot = []
postOrder (Node n lf rt) = postOrder lf ++ postOrder rt ++ [n]

-- Function to sort a list by taking help of a tree
treeSort :: (Ord a) => [a] -> [a]
treeSort lst = inOrder $ fromListLf $ treeIfy lst

-- Functor for the BST type
instance Functor Tree where
        fmap f NoRoot             = NoRoot
        fmap f (Node n lf rt)     = Node (f n) (fmap f lf) (fmap f rt)
