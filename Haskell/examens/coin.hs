
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
    | x > n = []
    | otherwise = changesWithCoin ++ changesWithoutCoin
        where 
            changesWithCoin = map (x:) (changes (x:xs) (n - x))
            changesWithoutCoin = changes xs n

amountsEuroRev :: [Int]
amountsEuroRev = reverse amountsEuro

changesEuroRev :: Int -> [[Int]]
changesEuroRev = changes amountsEuroRev

checkReverse :: Int -> Bool
checkReverse i = length (changesEuro i) == length (changesEuroRev i)


sublistSum :: [Int] -> Int -> [[Int]]
sublistSum _ 0 = [[]]
sublistSum [] _ = []
sublistSum (x:xs) n 
    | n < 0 = []
    | x > n = []
    | otherwise = sublistWithX ++ sublistWOX
        where
            sublistWithX = map (x:) (sublistSum xs (n - x))
            sublistWOX = sublistSum xs n

powerSet :: [Int] -> [[Int]]
powerSet [] = [[]]
powerSet (x:xs) = powerSetWith ++ powerSetWO
    where
        powerSetWO = powerSet xs
        powerSetWith = map (x:) (powerSet xs)