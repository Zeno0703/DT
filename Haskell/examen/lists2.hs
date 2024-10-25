
module Template where

-- ----------------------------------------------------------------------------

delete5 :: [Integer] -> [Integer]
delete5 list = [x | x <- list, x /= 5]

dropAlternating :: Bool -> [a] -> [a]
dropAlternating _ [] = []
dropAlternating b (x:xs)
    | b = dropAlternating (not b) xs
    | otherwise = x: dropAlternating (not b) xs

accumulatedSum :: Integer -> [Integer] -> [Integer]
accumulatedSum n list =  n : [n + sum [1..x] | x <- list]

commaTrail :: [String] -> String
commaTrail list = concat (map (\s -> s ++ ",") list)

trailBy :: String -> [String] -> String
trailBy char list = concat (map (\s -> s ++ char) list)

commaSeparate :: [String] -> String
commaSeparate list = concat ((map (\s -> s ++ ",") (take n list)) ++ drop n list)
    where
        n = length list - 1

separateBy :: String -> [String] -> String
separateBy char list = concat ((map (\s -> s ++ char) (take n list)) ++ drop n list)
    where
        n = length list - 1
