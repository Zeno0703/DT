module MyHaskell where

import Data.List

-- 1.1
data LineItem =     Space  |
                    Newline |
                    Word String
    deriving (Eq)

-- 1.2
mkSpace :: LineItem
mkSpace = Space

mkNewline :: LineItem
mkNewline = Newline

mkWord :: String -> LineItem
mkWord = Word

-- 1.3
lineItemToStr :: LineItem -> String
lineItemToStr Space = show " "
lineItemToStr Newline = show "\n"
lineItemToStr (Word s) = show s

instance Show LineItem where
    show = lineItemToStr

-- 1.4

toLineItems :: String -> [LineItem]
toLineItems s = toLine (toList s)

toList :: String -> [String]
toList [] = []
toList (s:ss)
    | s == ' ' = [s] : toList ss
    | s == '\n' = "\n" : toList ss
    | otherwise = (s : takeWhile (\c -> c `notElem` [' ', '\n']) ss) : toList (dropWhile (\c -> c `notElem` [' ', '\n']) ss)

toLine :: [String] -> [LineItem]
toLine [] = []
toLine (l:ls)
            | l == " " = mkSpace : toLine ls
            | l == "\n" = mkNewline : toLine ls
            | otherwise = mkWord l : toLine ls

-- 1.5
fromLineItems :: [LineItem] -> String
fromLineItems = concatMap fromLineItem
  where
    fromLineItem Space    = " "
    fromLineItem Newline  = "\n"
    fromLineItem (Word s) = s

-- 2.1
removeSpaces :: [LineItem] -> [LineItem]
removeSpaces li = [i | i <- li, i /= Space]

-- 2.2
splitInLines :: [LineItem] -> [[LineItem]]
splitInLines [] = []
splitInLines (l:ls) 
                    | l == Newline || l == mkNewline = [] : splitInLines ls
                    | otherwise = [takeWhile (\s -> s /= Newline) (l:ls)] ++ splitInLines (dropWhile (\s -> s /= Newline) (l:ls))

-- 2.3
separateTooLongWords :: Int -> [LineItem] -> [[LineItem]]
separateTooLongWords = error "Not implemented"

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
