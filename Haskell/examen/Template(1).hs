
module Template where

-- * List Operations
-- ----------------------------------------------------------------------------

count :: [Int] -> Int
count [] = 0
count(x:xs) = 1 + count xs

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

append :: [Int] -> [Int] -> [Int]
append x y = x ++ y