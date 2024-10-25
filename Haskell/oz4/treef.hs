module Template where

data Tree a = Leaf a | Fork (Tree a) (Tree a)
  deriving (Show, Eq)

foldTree :: (a -> b) -> (b -> b -> b) -> (Tree a -> b)
foldTree f g (Leaf x) = f x
foldTree f g (Fork a b) = g (foldTree f g a) (foldTree f g b)

sumTree :: Tree Int -> Int
sumTree = foldTree id (+)
-- We willen de waarde gewoon behouden

treeToList :: Tree a -> [a]
treeToList = foldTree (\e -> [e]) (++)

nrOfLeaves :: Tree a -> Int
nrOfLeaves = foldTree (\e -> 1) (+)

depthOfTree :: Tree a -> Int
depthOfTree = foldTree (\e -> 1) (\x y -> max x y + 1)

mirrorTree :: Tree a -> Tree a
mirrorTree = foldTree (\x -> Leaf x) (\t1 t2 -> Fork t2 t1)

minTree :: Tree Int -> Int
minTree = foldTree id min

addOne :: Tree Int -> Tree Int
addOne = foldTree (\x -> Leaf (x + 1)) (\t1 t2 -> Fork t1 t2)

idTree :: Tree a -> Tree a
idTree = foldTree (\x -> Leaf (x)) (\t1 t2 -> Fork t1 t2)
