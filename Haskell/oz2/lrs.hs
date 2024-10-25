module Template where

import Data.List (delete)
import Control.Monad (when)
import Data.Binary.Get (label)
import Control.DeepSeq (rnf2)

-- * Selection Sort
-- ----------------------------------------------------------------------------

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort l =  x: selectionSort (delete x l)
    where
        x = minimum l


-- * Quicksort
-- ----------------------------------------------------------------------------

partitionFold :: (a -> Bool) -> [a] -> ([a],[a])
partitionFold p = foldr (\x (r1, r2) -> if p x then ([x] ++ r1, r2) else (r1, [x] ++ r2)) ([], [])

partitionFilter :: (a -> Bool) -> [a] -> ([a],[a])
partitionFilter p l = (r1, r2)
    where 
        r1 = filter p l
        r2 = filter (not . p) l

partitionLC :: (a -> Bool) -> [a] -> ([a],[a])
partitionLC p l = (r1, r2)
    where 
        r1 = [x | x <- l, p x]
        r2 = [x | x <- l, not $ p x]

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort r1 ++ [x] ++ quicksort r2
    where 
        (r1, r2) = partitionFilter (<=x) xs