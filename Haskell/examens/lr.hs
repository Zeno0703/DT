module Template where

import Data.List (delete)

-- * Selection Sort
-- ----------------------------------------------------------------------------

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort list = x : selectionSort newList
    where
        x = minimum list
        newList = delete x list

-- * Quicksort
-- ----------------------------------------------------------------------------

partitionFold :: (a -> Bool) -> [a] -> ([a],[a])
partitionFold p = foldr (\x (l, r) -> if p x then (x:l, r) else (l, x:r)) ([], [])

partitionFilter :: (a -> Bool) -> [a] -> ([a],[a])
partitionFilter p list = (yes, no)
    where
        yes = filter p list
        no = filter (not . p) list

partitionLC :: (a -> Bool) -> [a] -> ([a],[a])
partitionLC p list = ([x | x <- list, p x], [x | x <- list, not $ p x])

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort lower ++ [x] ++ quicksort higher
    where
        (lower, higher) = partitionLC (< x) xs