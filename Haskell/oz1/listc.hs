
module Template where

-- * List Operations
-- ----------------------------------------------------------------------------

factorial :: Integer -> Integer
factorial n = product [1..n]

myRepeat :: Int -> Int -> [Int]
myRepeat n x
    | n < 0 = []
    | otherwise = [x | _ <- [1..n]]

flatten :: [[Int]] -> [Int]
flatten list = [x | sublist <- list, x <- sublist]

range :: Int -> Int -> [Int]
range low high = [low..high]

sumInts :: Int -> Int -> Int
sumInts low high
    | low > high = 0
    | otherwise = sum (range low high)

removeMultiples :: Int -> [Int] -> [Int]
removeMultiples n list = [x | x <- list, (x `mod` n) /= 0]