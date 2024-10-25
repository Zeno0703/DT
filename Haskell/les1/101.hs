
module Template where

-- * Haskell 101
-- ----------------------------------------------------------------------------

double :: Int -> Int
double x = 2 * x

myAbs :: Int -> Int
myAbs x  
    | x < 0 = -1 * x
    | otherwise = x

toFahrenheit :: Float -> Float
toFahrenheit x = 1.8 * x + 32.0

fizzbuzz :: Int -> String
fizzbuzz x 
    | mod x 3 == 0 && mod x 5 == 0 = "fizzbuzz"
    | mod x 3 == 0 = "fizz"
    | mod x 5 == 0 = "buzz"
    | otherwise = show x