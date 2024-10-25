
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
externalSenders = map (\(MkServer domain emails) -> MkReport domain (filter (\(MkEmail name ext) -> ext /= domain) emails))

-- ----------------------------------------------------------------------------

unique :: [Integer] -> [Integer]
unique = foldl (\r x -> if x `elem` r then r else r ++ [x]) []

-- ----------------------------------------------------------------------------

type Letter = Char
type Author = String

data Shelf = MkShelf Letter [Author]
  deriving (Eq, Show)

fixShelves :: [Shelf] -> [Shelf]
fixShelves = error "Not implemented"

-- ----------------------------------------------------------------------------

elimContainsChar :: Char -> [String] -> [String]
elimContainsChar c = filter (\s -> c `notElem` s)

-- ----------------------------------------------------------------------------

leetSpeak :: [String] -> [String]
leetSpeak = map (map leet)

leet :: Char -> Char
leet a
    | a == 'a' || a == 'A' = '4'
    | a == 'e' || a == 'E' = '3'
    | a == 'i' || a == 'I' = '1'
    | a == 'o' || a == 'O' = '0'
    | a == 't' || a == 'T' = '7'
    | otherwise = a

-- ----------------------------------------------------------------------------

isPalindrome :: String -> Bool
isPalindrome "" = True
isPalindrome s = head string == last string && isPalindrome (drop 1 $ init string)
    where
        string = filter isAlphaNum s

-- ----------------------------------------------------------------------------

type Species = String
type Length  = Float     -- in inches

data Fish = MkFish Species Length
  deriving (Eq, Show)

viableFish :: [Fish] -> [String] -> [Fish]
viableFish = error "Not implemented"

