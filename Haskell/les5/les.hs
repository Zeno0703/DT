import Data.Char

inflation :: [[Int]] -> [[Int]]
inflation list = map (map (* 2)) (filter (\l -> sum l > 10) list)

fullReverse :: [[a]] -> [[a]]
fullReverse list = reverse (map reverse list)

-- [[1, 2, 3], [4], [5, 6]] -> [[2, 3, 4], [5], [6, 1]]

rotate :: [[a]] -> [[a]]
rotate l = structure l l'
    where
        l' = concat l

rot :: [a] -> [a]
rot (x:xs) = xs ++ [x]

structure :: [[a]] -> [b] -> [[b]]
structure [] [] = []
structure (x:xs) ys = take n ys : structure xs (drop n ys)
    where n = length x

data Document = MkDoc Title Content
type Title = String
type Content = [Item]
data Item = Part Title Content | Text String

mkDocument :: Title -> Content -> Document
mkDocument = MkDoc

mkPart :: Title -> Content -> Item
mkPart = Part

mkText :: String -> Item
mkText = Text

prod :: [Int] -> Int
prod [] = 1
prod (x:xs) = x * prod xs

calculator1 :: IO ()
calculator1 = do    let stack = []
                    calc stack

calc :: [Int] -> IO ()
calc stack = do     num <- getLine
                    let action 
                            | num == "stop" = putStrLn "Goodbye!"
                            | num == "+" = print (sum stack)
                            | num == "*" = print (prod stack)
                            | num == "pop" = print (take n stack)
                            | otherwise = do    let s = [read num :: Int] ++ stack
                                                print s
                                                calc s
                    action
                    where n = length stack - 1