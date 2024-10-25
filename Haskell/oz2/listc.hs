
module Template where

-- * List Comprehensions
-- ----------------------------------------------------------------------------

mapLC :: (a -> b) -> [a] -> [b]
mapLC f l = [f x | x <- l]

filterLC :: (a -> Bool) -> [a] -> [a]
filterLC f l = [x | x <- l, f x]