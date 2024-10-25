
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
externalSenders = error "Not implemented"

-- ----------------------------------------------------------------------------

unique :: [Integer] -> [Integer]
unique = error "Not implemented"

-- ----------------------------------------------------------------------------

type Letter = Char
type Author = String

data Shelf = MkShelf Letter [Author]
  deriving (Eq, Show)

fixShelves :: [Shelf] -> [Shelf]
fixShelves = error "Not implemented"

-- ----------------------------------------------------------------------------

elimContainsChar :: Char -> [String] -> [String]
elimContainsChar = error "Not implemented"

-- ----------------------------------------------------------------------------

leetSpeak :: [String] -> [String]
leetSpeak = error "Not implemented"

-- ----------------------------------------------------------------------------

isPalindrome :: String -> Bool
isPalindrome = error "Not implemented"

-- ----------------------------------------------------------------------------

type Species = String
type Length  = Float     -- in inches

data Fish = MkFish Species Length
  deriving (Eq, Show)

viableFish :: [Fish] -> [String] -> [Fish]
viableFish = error "Not implemented"

