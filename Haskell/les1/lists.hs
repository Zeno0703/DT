
module Template where

-- * List Operations
-- ----------------------------------------------------------------------------

count :: [Int] -> Int
count list
    | null list = 0
    | otherwise = 1 + count (tail list)

myAnd :: [Bool] -> Bool
myAnd list
    | null list = True
    | otherwise = head list && myAnd (tail list) 

myOr :: [Bool] -> Bool
myOr list
    | null list = False
    | otherwise = head list || myOr (tail list) 

append :: [Int] -> [Int] -> [Int]
append list1 list2 = list1 ++ list2

