
module Template where

-- * Function Chaining
-- ----------------------------------------------------------------------------

applyAll :: [a -> a] -> a -> a
applyAll fList n = foldr (\x r -> x r) n fList

applyTimes :: Int -> (a -> a) -> a -> a
applyTimes n f= applyAll [f | s <- [1..n]]

applyMultipleFuncs :: a -> [a -> b] -> [b]
applyMultipleFuncs n f = [func n | func <- f]