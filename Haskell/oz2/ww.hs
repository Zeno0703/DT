module MyHaskell where

-- 1.1
data LineItem = Space String| Newline String | Word String
    deriving (Eq)

-- 1.2
mkSpace :: LineItem
mkSpace = Space " "

mkNewline :: LineItem
mkNewline = Newline "\n"

mkWord :: String -> LineItem
mkWord = Word

-- 1.3
lineItemToStr :: LineItem -> String
lineItemToStr (Space s) = "\"" ++ s ++ "\""
lineItemToStr (Newline n) = "\"" ++ n ++ "\""
lineItemToStr (Word w) = "\"" ++ w ++ "\""


instance Show LineItem where
  show (Space s) = lineItemToStr (Space s)
  show (Newline s) = lineItemToStr (Newline s)
  show (Word s) = lineItemToStr (Word s)

-- 1.4
toLineItems :: String -> [LineItem]
toLineItems str = reverse $ foldl f [] str
  where
    f r ' ' = mkSpace : r
    f r '\n' = mkNewline : r
    f (Word w : r) char = Word (w ++ [char]) : r
    f r char = mkWord [char] : r

-- 1.5
fromLineItems :: [LineItem] -> String
fromLineItems = foldr f []
    where
        f (Space s) r = s ++ r
        f (Newline s) r  = s ++ r
        f (Word s) r = s ++ r


-- 2.1
removeSpaces :: [LineItem] -> [LineItem]
removeSpaces = filter (/= mkSpace)

-- 2.2
splitInLines :: [LineItem] -> [[LineItem]]
splitInLines [] = []
splitInLines list 
    | null after = [before]
    | otherwise = before : splitInLines (tail after)
    where
        (before, after) = break (== mkNewline) list

-- 2.3
separateTooLongWords :: Int -> [LineItem] -> [[LineItem]]
separateTooLongWords n list = map (\(Word s) -> if length s > n then [mkWord (take n s)], [mkWord (drop n s)] else [mkWord s])

-- 2.4
wrap :: Int -> [LineItem] -> [[LineItem]]
wrap = error "Not implemented"

-- 2.5
joinLineWithSpaces :: [LineItem] -> [LineItem]
joinLineWithSpaces = error "Not implemented"

-- 2.6
joinLinesWithNewlines :: [[LineItem]] -> [LineItem]
joinLinesWithNewlines = error "Not implemented"

-- DO NOT CHANGE THIS FUNCTION
wordWrap :: Int -> String -> String
wordWrap lineWidth =
    fromLineItems .
    joinLinesWithNewlines .
    map joinLinesWithNewlines .
    map (map joinLineWithSpaces . concatMap (wrap lineWidth)) .
    map (separateTooLongWords lineWidth) .
    splitInLines .
    removeSpaces .
    toLineItems


-- 3.1
getLines :: IO String
getLines = error "Not implemented"

-- 3.2
interactiveWrapper :: IO ()
interactiveWrapper = error "Not implemented"
