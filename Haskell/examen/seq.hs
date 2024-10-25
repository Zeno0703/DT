
module Template where

import Data.Char

-- * Sequences
-- ----------------------------------------------------------------------------

class Sequence a where
  prev :: a -> a
  next :: a -> a

instance Sequence Integer where
  prev x = x - 1 
  next x = x + 1

instance Sequence Char where
  prev c
    | c == 'a' = error "no value before 'a'"
    | otherwise = chr (ord c - 1)
  next c
    | c == 'z' = error "no value after 'z'"
    | otherwise = chr (ord c + 1)

instance Sequence Bool where
  prev b = not b
  next b = not b

class Sequence a => LeftBoundedSequence a where
  firstElem :: a

class Sequence a => RightBoundedSequence a where
  lastElem :: a

instance LeftBoundedSequence Char where
  firstElem = 'a'

instance LeftBoundedSequence Bool where
  firstElem = False

instance RightBoundedSequence Char where
  lastElem = 'z'

instance RightBoundedSequence Bool where
  lastElem = True