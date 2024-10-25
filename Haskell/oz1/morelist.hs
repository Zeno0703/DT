module Template where

-- ----------------------------------------------------------------------------

delete5 :: [Integer] -> [Integer]
delete5 = filter (/= 5)

dropAlternating :: Bool -> [a] -> [a]
dropAlternating _ [] = []
dropAlternating b (x:xs)
    | b = dropAlternating (not b) xs
    | otherwise = x: dropAlternating (not b) xs

accumulatedSum :: Integer -> [Integer] -> [Integer]
accumulatedSum n [] = [n]
accumulatedSum n (x:xs) = n : accumulatedSum (n+x) xs

commaTrail :: [String] -> String
commaTrail [] = ""
commaTrail list = concat [word ++ "," | word <- init list] ++ last list ++ ","

trailBy :: String -> [String] -> String
trailBy char [] = ""
trailBy char list = concat [word ++ char | word <- init list] ++ last list ++ char

commaSeparate :: [String] -> String
commaSeparate [] = ""
commaSeparate list = concat [word ++ "," | word <- init list] ++ last list

separateBy :: String -> [String] -> String
separateBy char [] = ""
separateBy char list= concat [word ++ char | word <- init list] ++ last list