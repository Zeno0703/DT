
module Template where

-- * Folds
-- ----------------------------------------------------------------------------

mySum :: [Integer] -> Integer
mySum = foldInts (+) 0

myProduct :: [Integer] -> Integer
myProduct = foldInts (*) 1

foldInts :: (Integer -> Integer -> Integer) -> Integer -> [Integer] -> Integer
foldInts fn base [] = base
foldInts fn base (i:is) = fn i (foldInts fn base is)

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl fn base [] = base
myFoldl fn base (x:xs) = myFoldl fn (fn base x) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr fn base [] = base
myFoldr fn base (x:xs) = fn x (myFoldr fn base xs)

readInBase :: Int -> [Int] -> Int
readInBase base digits = myFoldl (\r x -> base * r + x) 0 digits

myMap :: (a -> b) -> [a] -> [b]
myMap fn [] = []
myMap fn (x:xs) = fn x : myMap fn xs

myMapF :: (a -> b) -> [a] -> [b]
myMapF fn = foldr (\x r -> fn x : r) []

