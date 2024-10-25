module MyHaskell where

import Data.List

-- Task 1a

data Circuit
  = Input String |
    NOT Circuit |
    AND Circuit Circuit|
    OR Circuit Circuit|
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
example = cor (cand (cinput "x") (cinput "y")) (cxor (cnot (cinput "z")) (cinput "x"))

-- Task 1d

candMany :: [Circuit] -> Circuit
candMany [x] = x
candMany (c:cs) = cand c (candMany cs)

-- Task 2a

instance Show Circuit where
  show (Input s) = s
  show (NOT x) = "NOT(" ++ show x ++ ")"
  show (AND x y) = "AND(" ++ show x ++ "," ++ show y ++ ")"
  show (OR x y) = "OR(" ++ show x ++ "," ++ show y ++ ")"
  show (XOR x y) = "XOR(" ++ show x ++ "," ++ show y ++ ")"

-- Task 2b

simplify :: Circuit -> Circuit
simplify (Input s) = cinput s
simplify (NOT x) = cnot (simplify x)
simplify (AND x y) = cand (simplify x) (simplify y)
simplify (OR x y) = cnot (cand (cnot (simplify x)) (cnot (simplify y)))
simplify (XOR x y) = simplify (cor (cand (simplify x) (cnot (simplify y))) (cand (cnot (simplify x)) (simplify y)))


-- Task 2c

size :: Circuit -> Int
size (Input s) = 0
size (AND x y) = 1 + size x + size y
size (NOT x) = 1 + size x
size (OR x y) = 1 + size x + size y
size (XOR x y) = 1 + size x + size y


-- Task 2d

gateDelay :: Circuit -> Int
gateDelay (Input s) = 0
gateDelay (AND x y) = 0 + gateDelay x + gateDelay y
gateDelay (NOT x) = 1 + gateDelay x
gateDelay (OR x y) = 1 + gateDelay x + gateDelay y
gateDelay (XOR x y) = 1 + gateDelay x + gateDelay y

-- Task 2e

inputs :: Circuit -> [String]
inputs (Input s) = [s]
inputs (NOT x) = inputs x
inputs (AND x y) = sort (removeDuplicates (inputs x ++ inputs y))
inputs (OR x y) = sort (removeDuplicates (inputs x ++ inputs y))
inputs (XOR x y) = sort (removeDuplicates (inputs x ++ inputs y))

removeDuplicates :: [String] -> [String]
removeDuplicates = foldr (\x r -> if x `elem` r then r else x:r) []

-- Task 3a

simulate :: Circuit -> [(String,Bool)] -> Bool
simulate (Input s) [(a, b)] = if s == a then b else not b
simulate (NOT x) [(a, b)] = not (simulate x [(a, b)])
simulate (AND x y) [(a, b), (c, d)] = simulate x [(a, b)] && simulate y [(c, d)]
simulate (OR x y) [(a, b), (c, d)] = simulate x [(a, b)] || simulate y [(c, d)]
simulate (XOR x y) [(a, b), (c, d)] = simulate x [(a, b)] `xor` simulate y [(c, d)]

xor a b = a /= b 

-- Task 3b

combinations :: Int -> [[Bool]]
combinations = undefined

-- Task 3c
tabulate :: Circuit -> IO ()
tabulate = undefined


-- -- Task 4a
-- 
-- check :: Circuit -> [(String,Bool)] -> Bool -> Bool
-- check c env r = undefined
-- 
-- checkAll :: [([(String,Bool)],Bool)] -> Circuit -> Bool
-- checkAll = undefined
-- 
-- -- Task 4b
-- 
-- splits :: Int -> [(Int,Int)]
-- splits = undefined
-- 
-- -- Task 4c
-- 
-- generate :: [String] -> Int -> [Circuit]
-- generate = undefined
-- 
-- -- Task 4d
-- 
-- smallest :: [String] -> [([(String,Bool)],Bool)] -> Circuit
-- smallest = undefined

