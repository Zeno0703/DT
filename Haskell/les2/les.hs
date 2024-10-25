count :: [Int] -> Int
count = foldr (\l x -> x + 1) 0

add :: [Int] -> Int
add = foldr (+) 0

addAndCount :: [Int] -> (Int, Int)
addAndCount = foldr f (0,0)
    where 
        f x (s, c) = (1 + c, x + s)

-- addAndCount (x:xs) = (x + s, 1 + c)
--     where
--         (s, c) = fusionHelp xs

-- split :: [Int] -> ([Int], [Int])
-- split [] = ([], [])
-- split (x:xs) = (zs, x:ys)
--     where 
--         (ys, zs) = split xs

split' :: [Int] -> ([Int], [Int])
split' = foldr f ([], [])
    where
        f x (ys, zs) = (zs, x:ys)

product' :: [Int] -> Int
product' = foldr (*) 1

tuple :: [a] -> (b -> [(a, b)])
tuple [] y = []
tuple (x:xs) y = (x, y) : tuple xs y

tuple' :: [a] -> (b -> [(a, b)])
tuple' l y = foldr (\x r -> (x, y) : r) [] l 

-- tuple'' :: [a] -> (b -> [(a, b)])
-- tuple'' l y = map (\x -> (x, y)) l

tails :: [a] -> [[a]]
tails = foldr f []
    where
        f x (r:rs) = (x:r) : (r:rs)

-- tails :: [a] -> [[a]]
-- tails = foldr f []
--     where
--         f x l@(r:_) = (x:r) : l

-- r:rs zijn de lijsten reeds gegenereerd, we nemen een nieuw huidig element (vroeger in de lijst) en voegen die toe aan r (het element in ervoor in de genereerde lijst)
-- altijd huidig element in f => x

mts :: [Int] -> Int
mts l = maximum $ map sum $ tails l

-- (tails l) omdat tails een fu,nctie op l is, de haakjes onderscheiden -> map sum l, maar map sum (tails l)

data Tree = Leaf Int | Split Tree Tree

-- addTree :: Boom -> Int