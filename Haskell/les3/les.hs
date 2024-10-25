data Pet = Mouse | Dog | Elephant -- deriving Show (kan ook)

instance Eq Pet where  
    Mouse == Mouse = True
    Dog == Dog = True
    Elephant == Elephant = True
    _ == _ = False
    -- e1 == e2 =  show e1 == show e2

instance Show Pet where
    show Mouse = "Muizeke"
    show Dog = "Daggoe"
    show Elephant = "Kevin"

instance Ord Pet where
    compare Mouse Mouse = EQ
    compare Dog Dog = EQ
    compare Elephant Elephant = EQ
    compare Elephant _ = GT
    compare _ Elephant = LT
    compare Mouse _ = LT
    compare _ Mouse = GT

{-
instance Ord Pet where
    Dog <= Elephant = True
    Mouse <= Dog = True
    Mouse <= Elephant = True
    a1 <= a2 = a1 == a2
-}

data Point = MkPoint Int Int

instance Eq Point where
    MkPoint a b == MkPoint c d = a == c && b == d

instance Show Point where
    show (MkPoint a b) = "( " ++ show a ++ ", " ++ show b ++ " )"

instance Ord Point where
    MkPoint a b <= MkPoint c d = (a * a) + (b * b) <= (c * c) + (d * d)

-- insert :: [Int] -> Int -> [Int]
-- insert [] y = [y]
-- insert (x:xs) y 
--     | x <= y = x: insert xs y
--     | otherwise = y : x : xs

insert :: Ord a => [a] -> a -> [a]
insert [] y = [y]
insert (x:xs) y 
    | x <= y = x: insert xs y
    | otherwise = y : x : xs