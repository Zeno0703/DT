
module Template where

-- * Rock - Paper - Scissors
-- ----------------------------------------------------------------------------

data Move = Rock | Paper | Scissors
  deriving (Eq, Show)

beat :: Move -> Move
beat m
    | m == Scissors = Rock
    | m == Rock = Paper
    | otherwise = Scissors

lose :: Move -> Move
lose m
    | m == Rock = Scissors
    | m == Paper = Rock
    | otherwise = Paper

data Result = Win | Lose | Draw
  deriving (Eq, Show)

outcome :: Move -> Move -> Result
outcome m1 m2
    | lose m1 == m2 = Win
    | beat m1 == m2 = Lose
    | otherwise = Draw