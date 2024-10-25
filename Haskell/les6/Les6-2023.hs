import Data.Char (isDigit)

type Time     = Int
type Distance = Int

-- 1. tb    : knop gedrukt houden
-- 1. t - tb: verplaatst aan tb millimeter/milliseconde

-- gamePossibilities 3
--   1    2    3
--   2*1  1*2  0 
--   [2,2,0]
gamePossibilities :: Time -> [Distance] 
gamePossibilities t = [tb * (t - tb) | tb <- [0..t]]

winningPossibilities :: Time -> Distance -> Int
winningPossibilities t bd = 
  length (filter (>bd) (gamePossibilities t))

main :: IO ()
main = readFile "testdata1.txt" >>= \s -> print (process s)

-- product :: Num a => [a] -> a
-- read    :: Read a => String -> a
-- lines   :: String -> [String] 
-- words   :: String -> [String]
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

process :: String -> Int
process s = 
  let [t,bd] = map read (map (filter isDigit) (lines s))
  in winningPossibilities t bd


{-
data Trie a
  = Leaf
  | Accept [(a,Trie a)]
  | Reject [(a,Trie a)]

leaf   :: Trie a
leaf = Leaf

accept :: [(a,Trie a)] -> Trie a
accept = Accept

reject :: [(a,Trie a)] -> Trie a
reject = Accept
-}

data Trie a
 = Node Bool [(a,Trie a)]

leaf   :: Trie a
leaf = node True []

accept :: [(a,Trie a)] -> Trie a
accept = node True

reject :: [(a,Trie a)] -> Trie a
reject = node False

node   :: Bool -> [(a,Trie a)] -> Trie a
node = Node

mytrie :: Trie Int
mytrie = 
  reject [(1, accept [(5, leaf)
                     ,(7, leaf)])
         ,(3, leaf)
         ,(7, reject [(1, leaf)])]

height :: Trie a -> Int
height (Node _ []) = 1
height (Node _ xs) = 1 + maximum [height t | (_,t) <- xs]

{-
lookup :: Eq a => a -> [(a,b)] -> Maybe b
lookup k [] = Nothing
lookup k ((k',v):l)
  | k == k'    =  Just v
  | otherwise  =  lookup k l
-}

hasPath :: Eq a => [a] -> Trie a -> Bool
hasPath []     (Node b _)   = b
hasPath (x:xs) (Node _ ts)  = 
  case lookup x ts of
    Nothing -> False
    Just t  -> hasPath xs t

-- unlines :: [String] -> String
instance Show a => Show (Trie a) where
  show = unlines . showTrieLines

showTrieLines :: Show a => Trie a -> [String]
showTrieLines (Node b []) = [if b then "x" else "o"]
showTrieLines (Node b ts) = l1 : concat [ addLabel x (showTrieLines t) | (x,t) <- rest]
                                        ++ addLabelLast tlast
 where
  tlast = last ts
  rest  = init ts
  l1 = if b then "x" else "o"
  addLabel x (l:ls) = ("|-" ++ show x ++ "- " ++ l) : map ("|    "++) ls
  addLabelLast (x,t) = ("`-" ++ show x ++ "- " ++ l) : map ("     "++) ls
    where
      (l:ls) = showTrieLines t

{-

o
|-1- x
|    |-5- x
|    `-7- x
|-3- x
`-7- o
     `-1- x

 -}
