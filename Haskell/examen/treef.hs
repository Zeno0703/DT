module Template where

data Tree a = Leaf a | Fork (Tree a) (Tree a)
  deriving (Show, Eq)

foldTree :: (a -> b) -> (b -> b -> b) -> (Tree a -> b)
foldTree f g (Leaf a) = f a
foldTree f g (Fork tree1 tree2) = g (foldTree f g tree1) (foldTree f g tree2)

sumTree :: Tree Int -> Int
sumTree = foldTree id (+)

treeToList :: Tree a -> [a]
treeToList = foldTree (\v -> [v]) (++)

nrOfLeaves :: Tree a -> Int
nrOfLeaves = foldTree (\v -> 1) (+)

depthOfTree :: Tree a -> Int
depthOfTree = foldTree (\v -> 1) (\l r -> max l r + 1)

mirrorTree :: Tree a -> Tree a
mirrorTree = foldTree (\v -> Leaf v) (\l r -> Fork r l)

minTree :: Tree Int -> Int
minTree = foldTree id (\l r -> min l r)

addOne :: Tree Int -> Tree Int
addOne = foldTree (\v -> Leaf (v + 1)) (\l r -> Fork l r)

idTree :: Tree a -> Tree a
idTree = foldTree Leaf Fork
