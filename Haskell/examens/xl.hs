
module Template where

-- ----------------------------------------------------------------------------

delete5 :: [Integer] -> [Integer]
delete5 = filter (/= 5)

dropAlternating :: Bool -> [a] -> [a]
dropAlternating _ [] = []
dropAlternating b (x:xs)
    | b = dropAlternating (not b) xs
    | otherwise = x : dropAlternating (not b) xs

accumulatedSum :: Integer -> [Integer] -> [Integer]
accumulatedSum n [] = [n]
accumulatedSum n (x:xs) = n : accumulatedSum (n + x) xs

commaTrail :: [String] -> String
commaTrail = concatMap (\s -> s ++ ",")

trailBy :: String -> [String] -> String
trailBy c = concatMap (\s -> s ++ c)

commaSeparate :: [String] -> String
commaSeparate string = concatMap (\s -> s ++ ",") (take n string) ++ last string
    where
        n = length string - 1

separateBy :: String -> [String] -> String
separateBy c string = concatMap (\s -> s ++ c) (take n string) ++ last string
    where
        n = length string - 1


skew :: Int -> [[Int]] -> [[Int]]
skew n l = struct l l'
    where
        l' = rotate n (concat l)

rotate :: Int -> [Int] -> [Int]
rotate n l = drop n l ++ take n l

struct :: [[Int]] -> [Int] -> [[Int]]
struct [] [] = []
struct (x:xs) l = take n l : struct xs (drop n l)
    where
        n = length x