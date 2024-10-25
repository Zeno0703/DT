
module Template where

foldr' :: (Int -> a -> a) -> a -> [Int] -> a
foldr' f z []       = z
foldr' f z (x : xs) = f x (foldr' f z xs)

length' :: [Int] -> Int
length' = foldr' (\l x -> x + 1) 0

any' :: (Int -> Bool) -> [Int] -> Bool
any' pred = foldr' (\x xs -> pred x || xs) False

all' :: (Int -> Bool) -> [Int] -> Bool
all' pred = foldr' (\x xs -> pred x && xs) True

map' :: (Int -> Int) -> [Int] -> [Int]
map' f [] = []
map' f (x:xs) = f x: map' f xs

-- map' :: (Int -> Int) -> [Int] -> [Int]
-- map' f = foldr (\x acc -> f x : acc) []

filter' :: (Int -> Bool) -> [Int] -> [Int]
filter' p [] = []
filter' p (x:xs)
    | p x = x: filter' p xs
    | otherwise = filter' p xs

-- filter' :: (Int -> Bool) -> [Int] -> [Int]
-- filter' p = foldr (\x acc -> if p x then x:acc else acc) []

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
amountEven = length' . filter' even'

onlyOdd :: [Int] -> [Int]
onlyOdd = filter' (not' . even')

absGtFive :: [Int] -> Int
absGtFive = length' . filter' (greaterThanFive . absolute')

anyEvenGtFive :: [Int] -> Bool
anyEvenGtFive = any' (\x -> greaterThanFive x && even' x)

anyEvenGtFive' :: [Int] -> Bool
anyEvenGtFive' = foldr' (\x xs -> pred x || xs) False
    where pred x = greaterThanFive x && even' x




palindrome :: [Int] -> Bool
palindrome [] = True
palindrome [_] = True
palindrome (x:xs) = x == last xs && palindrome (init xs)