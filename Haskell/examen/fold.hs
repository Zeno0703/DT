
module Template where

foldr' :: (Int -> a -> a) -> a -> [Int] -> a
foldr' f z []       = z
foldr' f z (x : xs) = f x (foldr' f z xs)

length' :: [Int] -> Int
length' = foldr' (\x r -> r + 1) 0

any' :: (Int -> Bool) -> [Int] -> Bool
any' p = foldr' f False
    where
        f x r = p x || r

all' :: (Int -> Bool) -> [Int] -> Bool
all' p = foldr' f True
    where
        f x r = p x && r

map' :: (Int -> Int) -> [Int] -> [Int]
map' f = foldr' (\x r -> f x : r) []

filter' :: (Int -> Bool) -> [Int] -> [Int]
filter' p = foldr' f []
    where
        f x r 
            | p x = x : r
            | otherwise = r

-- * Given helpers

even' :: Int -> Bool
even' = even

not' :: Bool -> Bool
not' = not

absolute' :: Int -> Int
absolute' = abs

greaterThanFive :: Int -> Bool
greaterThanFive x = x > 5

-- * Beginning Composer

amountEven :: [Int] -> Int
amountEven = length' . filter' even 

onlyOdd :: [Int] -> [Int]
onlyOdd = filter' odd

absGtFive :: [Int] -> Int
absGtFive = length' . filter (>5) . map absolute'

anyEvenGtFive :: [Int] -> Bool
anyEvenGtFive = any' even . filter (>5) . map absolute'

anyEvenGtFive' :: [Int] -> Bool
anyEvenGtFive' l = length' f > 0 
    where
        f = filter even gr
        gr = filter (>5) ab
        ab = map absolute' l