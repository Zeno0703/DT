double :: Int -> Int
double x = x * 2

myAbs :: Int -> Int
myAbs x
    | x < 0 = -1 * x
    | otherwise = x

toFahrenheit :: Float -> Float
toFahrenheit x = 1.8 * x + 32

fizzbuzz :: Int -> String
fizzbuzz x
    | x `mod` 15 == 0 = "fizzbuzz"
    | x `mod` 3 == 0 = "fizz"
    | x `mod` 5 == 0 = "buzz"
    | otherwise = show x