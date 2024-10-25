
module Template where

-- * Function Chaining
-- ----------------------------------------------------------------------------

-- applyAll :: [a -> a] -> a -> a
-- applyAll [] n = n
-- applyAll (x:xs) n = x $ applyAll xs n

applyAll f n = foldr (\x r -> x r) n f

applyTimes :: Int -> (a -> a) -> a -> a
applyTimes n f = applyAll [f | i <- [1..n]]

applyMultipleFuncs :: a -> [a -> b] -> [b]
applyMultipleFuncs n f = [fn n | fn <- f]