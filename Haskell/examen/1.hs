
module Template where

-- ----------------------------------------------------------------------------

delete5 :: [Integer] -> [Integer]
delete5 = filter (/= 5)

dropAlternating :: Bool -> [a] -> [a]
dropAlternating _ [] = []
dropAlternating b (x:xs)
                    | b = dropAlternating (not b) xs
                    | not b = x: dropAlternating (not b) xs

accumulatedSum :: Integer -> [Integer] -> [Integer]
accumulatedSum n [] = [n]
accumulatedSum n (x:xs) = n : accumulatedSum (n+x) xs 

commaTrail :: [String] -> String
commaTrail slist = concat [s ++ "," | s <- slist]

trailBy :: String -> [String] -> String
trailBy char = concatMap (++ char) 

commaSeparate :: [String] -> String
commaSeparate slist = concat ([s ++ "," | s <- take n slist] ++ drop n slist)
            where
                n = length slist - 1

separateBy :: String -> [String] -> String
separateBy char slist = concat ([s ++ char | s <- take n slist] ++ drop n slist)
            where
                n = length slist - 1