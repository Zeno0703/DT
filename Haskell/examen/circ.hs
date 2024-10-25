module MyHaskell where
-- Task 1a

import Data.List

data Circuit
  = Input String  |
    NOT Circuit   |
    AND Circuit Circuit |
    OR Circuit Circuit |
    XOR Circuit Circuit

-- Task 1b

cinput :: String -> Circuit 
cinput = Input

cnot   :: Circuit -> Circuit
cnot   = NOT

cand   :: Circuit -> Circuit -> Circuit
cand   = AND

cor    :: Circuit -> Circuit -> Circuit
cor    = OR

cxor   :: Circuit -> Circuit -> Circuit
cxor   = XOR

-- Task 1c

example :: Circuit
example = cor (cand (cinput "x") (cinput "y")) (cxor (cinput "x") (cnot (cinput "z")))

-- Task 1d

candMany :: [Circuit] -> Circuit
candMany [c] = c
candMany (c:cs) = cand c (candMany cs)

-- Task 2a

instance Show Circuit where
  show (Input s) = s
  show (NOT c) = "NOT(" ++ show c ++ ")"
  show (AND c1 c2) = "AND(" ++ show c1 ++ "," ++ show c2 ++ ")"
  show (OR c1 c2) = "OR(" ++ show c1 ++ "," ++ show c2 ++ ")"
  show (XOR c1 c2) = "XOR(" ++ show c1 ++ "," ++ show c2 ++ ")"

-- Task 2b

simplify :: Circuit -> Circuit
simplify (Input s) = Input s
simplify (NOT c) = NOT c
simplify (AND c1 c2) = AND c1 c2
simplify (OR x y) = cnot (cand (cnot (simplify x)) (cnot (simplify y)))
simplify (XOR x y) = simplify (cor (cand (simplify x) (cnot (simplify y))) (cand (cnot (simplify x)) (simplify y)))

-- Task 2c

size :: Circuit -> Int
size (Input _) = 0
size (NOT c) = 1 + size c
size (AND c1 c2) = 1 + size c1 + size c2
size (OR c1 c2) = 1 + size c1 + size c2
size (XOR c1 c2) = 1 + size c1 + size c2


-- Task 2d

gateDelay :: Circuit -> Int
gateDelay (Input _) = 0
gateDelay (NOT c) = 1 + gateDelay c
gateDelay (AND c1 c2) = 0 + gateDelay c1 + gateDelay c2
gateDelay (OR c1 c2) = 1 + gateDelay c1 + gateDelay c2
gateDelay (XOR c1 c2) = 1 + gateDelay c1 + gateDelay c2

-- Task 2e

inputs :: Circuit -> [String]
inputs (Input s) = [s]
inputs (NOT c) = sort (removeDuplicates (inputs c))
inputs (AND c1 c2) = sort (removeDuplicates (inputs c1 ++ inputs c2))
inputs (OR c1 c2) = sort (removeDuplicates (inputs c1 ++ inputs c2))
inputs (XOR c1 c2) = sort (removeDuplicates (inputs c1 ++ inputs c2))

removeDuplicates :: [String] -> [String]
removeDuplicates = foldr (\x r -> if x `notElem` r then x:r else r) []

-- Task 3a

simulate :: Circuit -> [(String,Bool)] -> Bool
simulate (Input s) inp = snd (head $ filter (\(x, y) -> x == s) inp)
simulate (NOT c) inp = not $ simulate c inp
simulate (AND c1 c2) inp = simulate c1 inp && simulate c2 inp
simulate (OR c1 c2) inp = simulate c1 inp || simulate c2 inp
simulate (XOR c1 c2) inp = simulate c1 inp `xor''` simulate c2 inp

xor'' :: Bool -> Bool -> Bool
xor'' True a = not a
xor'' False a = a

-- Task 3b

combinations :: Int -> [[Bool]]
combinations 0 = [[]]
combinations n = [x:xs | x <- [False, True], xs <- combinations (n-1)]

-- Task 3c
tabulate :: Circuit -> IO ()
tabulate circuit = do
  let input = inputs circuit
  let combis = combinations (length input)
  putStrLn (unwords input ++ " | output")
  drawSol circuit input combis

drawSol :: Circuit -> [String] -> [[Bool]] -> IO ()
drawSol _ _ [] = return ()
drawSol c input (x:xs) = do
  let simulationValues = zip input x
  mapM_ (\(_, y) -> putStr (if y then "1 " else "0 ")) simulationValues
  putStr "| "
  let answer = simulate c simulationValues
  putStrLn (if answer then "1" else "0") 
  drawSol c input xs