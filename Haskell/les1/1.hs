sumList :: [Int] -> Int
sumList list 
    | null list = 0
    | otherwise = head list + sumList (tail list)

-- sumList [] = 0
-- sumList(x:xtail) = x + sumList xtail

count :: [Int] -> Int
count list
    | null list = 0
    | otherwise = 1 + count (tail list)

-- count [] = 0
-- count(x:xtail) = 1 + count xtail

average :: [Int] -> Int
average [] = 0 -- Base case, otherwise it will divide by zero, yields an error
average list = div (sumList list) (count list)

fusionHelp :: [Int] -> (Int, Int)
fusionHelp [] = (0, 0)
fusionHelp (x:xs) = (x + s, 1 + c)
    where
        (s, c) = fusionHelp xs

fusion :: [Int] -> Int
fusion [] = 0
fusion list = div a s
    where
        (a, s) = fusionHelp list

insert :: [Int] -> Int -> [Int]
insert [] n = [n]
insert (x:xs) n
    | n < x = n:x:xs
    | otherwise = x: insert xs n

insort :: [Int] -> [Int]
insort [] = []
insort (x:xs) = insert (insort xs) x
