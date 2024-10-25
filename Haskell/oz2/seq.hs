
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
  prev a = if a `elem` ['b'..'z'] then chr (ord a - 1) else error "no value before 'a'"
  next a = if a `elem` ['a'..'y'] then chr (ord a + 1) else error "no value after 'z'"

instance Sequence Bool where
  prev = not
  next = not

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

