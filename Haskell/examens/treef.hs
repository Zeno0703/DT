module Template where

data Tree a = Leaf a | Fork (Tree a) (Tree a)
  deriving (Show, Eq)

foldTree :: (a -> b) -> (b -> b -> b) -> (Tree a -> b)
foldTree f g (Leaf a) = f a
foldTree f g (Fork tree1 tree2) = g (foldTree f g tree1) (foldTree f g tree2) 

sumTree :: Tree Int -> Int
sumTree = foldTree id (+)

treeToList :: Tree a -> [a]
treeToList = foldTree (\x -> [x]) (++)

nrOfLeaves :: Tree a -> Int
nrOfLeaves = foldTree (\x -> 1) (+)

depthOfTree :: Tree a -> Int
depthOfTree = foldTree (\x -> 1) (\t1 t2 -> 1 + max t1 t2) 

mirrorTree :: Tree a -> Tree a
mirrorTree = foldTree Leaf (\l r -> Fork r l)

minTree :: Tree Int -> Int
minTree = foldTree id min

addOne :: Tree Int -> Tree Int
addOne = foldTree (\x -> Leaf (x + 1)) Fork

idTree :: Tree a -> Tree a
idTree = foldTree Leaf Fork
