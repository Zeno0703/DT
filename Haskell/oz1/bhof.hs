
module Template where

import Data.Char (isAlphaNum, toUpper, toLower)

-- ----------------------------------------------------------------------------

type Username = String
type Domain = String

data Email = MkEmail Username Domain
  deriving (Eq, Show)

data Server = MkServer Domain [Email]
  deriving (Eq, Show)

data Report = MkReport Domain [Email]
  deriving (Eq, Show)

externalSenders :: [Server] -> [Report]
externalSenders = map (\ (MkServer domain extensions) -> MkReport domain (filter (\ (MkEmail name email) -> email /= domain) extensions))

-- ----------------------------------------------------------------------------

unique :: [Integer] -> [Integer]
unique = foldl (\r x -> if all (/= x) r then r ++ [x] else r) []

-- ----------------------------------------------------------------------------

type Letter = Char
type Author = String

data Shelf = MkShelf Letter [Author]
  deriving (Eq, Show)

fixShelves :: [Shelf] -> [Shelf]
fixShelves shelves = filter (\ (MkShelf _ authors) -> not (null authors)) shelveList
    where
      shelveList = map (\ (MkShelf letter authors) -> MkShelf letter (filter (\a -> head a /= letter) authors)) shelves

-- ----------------------------------------------------------------------------

elimContainsChar :: Char -> [String] -> [String]
elimContainsChar c = filter (\x -> c `notElem` x)

-- ----------------------------------------------------------------------------

leetSpeak :: [String] -> [String]
leetSpeak = map (map leet)

leet :: Char -> Char
leet c
    | c == 'a' || c == 'A' = '4'
    | c == 'e' || c == 'E' = '3'
    | c == 'i' || c == 'I' = '1'
    | c == 'o' || c == 'O' = '0'
    | c == 't' || c == 'T' = '7'
    | otherwise = c

-- ----------------------------------------------------------------------------

isPalindrome :: String -> Bool
isPalindrome "" = True
isPalindrome s
  | head sFiltered /= last sFiltered = False
  | otherwise = isPalindrome rest
    where
      sFiltered = map toLower (filter isAlphaNum s)
      rest = drop 1 (take n sFiltered)
      n = length sFiltered - 1

-- ----------------------------------------------------------------------------

type Species = String
type Length  = Float     -- in inches

data Fish = MkFish Species Length
  deriving (Eq, Show)

viableFish :: [Fish] -> [String] -> [Fish]
viableFish fish invasive = filter (\ (MkFish species length) -> species `notElem` invasive && length > 8) fish