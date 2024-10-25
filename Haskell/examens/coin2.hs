
module Template where

amountsEuro :: [Int]
amountsEuro = [1, 2, 5, 10, 20, 50, 100, 200]

changesEuro :: Int -> [[Int]]
changesEuro = changes amountsEuro

changes :: [Int] -> Int -> [[Int]]
changes _ 0 = [[]]
changes [] _ = []
changes (x:xs) n 
    | n < 0 = []
    | otherwise = listWithCoin ++ listWithoutCoin
        where
            listWithCoin = map (x:) (changes (x:xs) (n - x))
            listWithoutCoin = changes xs n

amountsEuroRev :: [Int]
amountsEuroRev = reverse amountsEuro

changesEuroRev :: Int -> [[Int]]
changesEuroRev = changes amountsEuroRev

checkReverse :: Int -> Bool
checkReverse i = length (changesEuro i) == length (changesEuroRev i)

sublistSums :: [Int] -> Int -> [[Int]]
sublistSums _ 0 = [[]]
sublistSums [] _ = []
sublistSums (x:xs) n = sumWithX ++ sumWithoutX
        where
            sumWithX = map (x:) (sublistSums xs (n - x))
            sumWithoutX = sublistSums xs n

powerSet :: [Int] -> [[Int]]
powerSet [] = [[]]
powerSet (x:xs) = map (x:) (powerSet xs) ++ powerSet xs

sublistProduct :: [Int] -> Int -> [[Int]]
sublistProduct _ 1 = [[]]
sublistProduct [] _ = []
sublistProduct (x:xs) n
    | n `mod` x == 0 = withValue ++ withoutValue
    | otherwise = withoutValue
    where 
        withValue = map (x :) (sublistProduct xs (n `div` x))
        withoutValue = sublistProduct xs n