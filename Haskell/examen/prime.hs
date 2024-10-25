
module Template where

-- * Exercise 5: Prime Numbers
-- ----------------------------------------------------------------------------

sieve :: Int -> [Int]
sieve m = sieve' [2..m]
    where
        sieve' [] = []
        sieve' (x:xs) = x : sieve' [n | n <- xs, n `mod` x /= 0]

-- -------------------------
-- Some useful functions
-- -------------------------
sqrtMono :: Double -> Double
sqrtMono = sqrt

i2d :: Int -> Double
i2d = fromIntegral

floorMono :: Double -> Int
floorMono = floor
-- -------------------------

floorSquare :: Int -> Int
floorSquare n = error "Not implemented"

fastSieve :: Int -> [Int]
fastSieve n = error "Not implemented"