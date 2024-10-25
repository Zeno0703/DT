
module Template where

-- * Rock - Paper - Scissors
-- ----------------------------------------------------------------------------

data Move = Rock | Paper | Scissors
  deriving (Eq, Show)

beat :: Move -> Move
beat m
    | m == Rock = Paper
    | m == Paper = Scissors
    | otherwise = Rock

lose :: Move -> Move
lose m
    | m == Rock = Scissors
    | m == Scissors = Paper
    | otherwise = Rock

data Result = Win | Lose | Draw
  deriving (Eq, Show)

outcome :: Move -> Move -> Result
outcome m1 m2
    | beat m1 == m2 = Win
    | lose m1 == m2 = Lose
    | otherwise = Draw