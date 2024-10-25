
module Template where

import Data.Char
import Data.List

-- * Caesar Cipher
-- ----------------------------------------------------------------------------

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c = if isAlphaNum c then int2let $ (let2int c + n) `mod` 26 else c

encode :: Int -> String -> String
encode n = map (shift n)

table :: [Float]
table = [ 8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4
        , 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1 ]

percent :: Int -> Int -> Float
percent x y = (fromIntegral x * 100) / fromIntegral y

freqHelper :: Char -> [Int] -> [Int]
freqHelper x list
        | isLower x = take idx list ++ [list !! idx + 1] ++ drop (idx + 1) list
        | otherwise = list
        where
                idx = let2int x

freqs :: String -> [Float]
freqs s = map (percent n) (foldr freqHelper zeroes s)
        where
                zeroes = replicate 26 0
                n = length s

chisqr :: [Float] -> [Float] -> Float
chisqr l1 l2 = sum [(o - e) ** 2 / e | (o, e) <- zipped]
        where
                zipped = zip l1 l2

rotate :: Int -> [a] -> [a]
rotate n list = drop n list ++ take n list

crack :: String -> String
crack str = encode b str
    where
        table' = freqs str
        table'' = [ chisqr (rotate n table') table | n <- [0..25] ]
        b =  negate (maybe 0 id (elemIndex (minimum table'') table'')) `mod` 26

