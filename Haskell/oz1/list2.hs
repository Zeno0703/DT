module Template where

-- * List Operations
-- ----------------------------------------------------------------------------

myProduct :: [Integer] -> Integer
myProduct = foldr (*) 1

insert :: Int -> [Int] -> [Int]
insert n [] = [n]
insert n r@(x:xs)
    | n <= x = n:r
    | otherwise = x: insert n xs

myLast :: [Int] -> Int
myLast [x] = x
myLast (x:xs) = myLast xs