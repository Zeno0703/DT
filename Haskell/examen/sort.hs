module Template where

import Data.List (delete)

-- * Selection Sort
-- ----------------------------------------------------------------------------

selectionSort :: Ord a => [a] -> [a]
selectionSort l = x : selectionSort (delete x l)
    where
        x = minimum l

-- * Quicksort
-- ----------------------------------------------------------------------------

partitionFold :: (a -> Bool) -> [a] -> ([a],[a])
partitionFold p= foldr (\x (ys, zs) -> if p x then (x:ys, zs) else (ys, x:zs)) ([], [])

partitionFilter :: (a -> Bool) -> [a] -> ([a],[a])
partitionFilter p l = (ys, zs)
    where
        ys = filter p l
        zs = filter (not . p) l

partitionLC :: (a -> Bool) -> [a] -> ([a],[a])
partitionLC p l = ([x | x <- l, p x], [x | x <- l, not (p x)])

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort l1 ++ [x] ++ quicksort l2
    where
        (l1, l2) = partitionFilter (<=x) xs