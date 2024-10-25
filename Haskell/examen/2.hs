
module Template where

-- * List Operations
-- ----------------------------------------------------------------------------

factorial :: Integer -> Integer
factorial n = product [1..n]

myRepeat :: Int -> Int -> [Int]
myRepeat n x = [x | s <- [1..n]]

flatten :: [[Int]] -> [Int]
flatten list = [x | s <- list, x <- s]

range :: Int -> Int -> [Int]
range low high = [low..high]

sumInts :: Int -> Int -> Int
sumInts low high = sum (range low high)

removeMultiples :: Int -> [Int] -> [Int]
removeMultiples n = filter (\e -> e `mod` n /= 0)

