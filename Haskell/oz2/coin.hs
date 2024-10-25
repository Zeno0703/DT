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
    | otherwise = changeWithCoin ++ changeWithoutCoin
        where
            changeWithCoin = map (x :) (changes (x:xs) (n - x))
            changeWithoutCoin = changes xs n

amountsEuroRev :: [Int]
amountsEuroRev = reverse amountsEuro

changesEuroRev :: Int -> [[Int]]
changesEuroRev = changes amountsEuroRev

checkReverse :: Int -> Bool
checkReverse i = length (changesEuro i) == length (changesEuroRev i)
