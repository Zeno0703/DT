
module Template where

-- * List Comprehensions
-- ----------------------------------------------------------------------------

mapLC :: (a -> b) -> [a] -> [b]
mapLC f l = [f x | x <- l]

filterLC :: (a -> Bool) -> [a] -> [a]
filterLC p l = [x | x <- l, p x]