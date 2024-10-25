
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
shift _ ' ' = ' '
shift n l = int2let $ mod (let2int l + n) 26

encode :: Int -> String -> String
encode n = map (shift n)

table :: [Float]
table = [ 8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4
        , 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1 ]

percent :: Int -> Int -> Float
percent a b = (fromIntegral a / fromIntegral b) * 100

updateFreqs :: [Int] -> Char -> [Int]
updateFreqs list char
  | isLower char = take idx list ++ [list !! idx + 1] ++ drop (idx + 1) list
  | otherwise    = list
  where
    idx = let2int char


freqs :: String -> [Float]
freqs string = map (\x -> percent x (length string)) (foldl updateFreqs initial string)
    where
        initial = replicate 26 0

chisqr :: [Float] -> [Float] -> Float
chisqr o e = sum [(o - e)^2 / e | (o, e) <- zip o e]

rotate :: Int -> [a] -> [a]
rotate n list = drop n list ++ take n list

crack :: String -> String
crack str = encode b str
    where
        table' = freqs str
        table'' = [ chisqr (rotate n table') table | n <- [0..25] ]
        b =  negate (maybe 0 id (elemIndex (minimum table'') table'')) `mod` 26

