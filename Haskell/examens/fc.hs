
module Template where

-- * Function Chaining
-- ----------------------------------------------------------------------------

applyAll :: [a -> a] -> a -> a
applyAll list n = foldr (\f r -> f r) n list

applyTimes :: Int -> (a -> a) -> a -> a
applyTimes n f = applyAll fList
    where
        fList = [f | x <- [1..n]]

applyMultipleFuncs :: a -> [a -> b] -> [b]
applyMultipleFuncs n fList = [f n | f <- fList]

