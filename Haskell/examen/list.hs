
module Template where

-- * List Operations
-- ----------------------------------------------------------------------------

myProduct :: [Integer] -> Integer
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

insert :: Int -> [Int] -> [Int]
insert n [] = [n]
insert n (x:xs)
    | n <= x = n:x:xs
    | otherwise = x : insert n xs

myLast :: [Int] -> Int
myLast (x:xs)
    | null xs = x
    | otherwise = myLast xs