module Template where

data MyBool = MyTrue
            | MyFalse

data Exp = Const MyBool
         | And Exp Exp
         | Or Exp Exp

-- -------------------------------------------------
-- Equality Checking
-- -------------------------------------------------

instance Eq MyBool where
  MyTrue == MyTrue = True
  MyFalse == MyFalse = True
  _ == _ = False

instance Eq Exp where
  Const b == Const a = b == a
  And a b == And c d = a == c && b == d
  Or a b == Or c d = a == c && b == d
  _ == _ = False

-- -------------------------------------------------
-- Printing
-- -------------------------------------------------

instance Show MyBool where
  show MyFalse = "False"
  show MyTrue = "True"

instance Show Exp where
  show (Const b) = if b == MyTrue then "True" else "False"
  show (And a b) = show a ++ " && " ++ show b
  show (Or a b) = show a ++ " || " ++ show b

-- -------------------------------------------------
-- Evaluating
-- -------------------------------------------------

class Evaluatable a where
  eval :: a -> Bool

instance Evaluatable MyBool where
  eval MyTrue = True
  eval MyFalse = False

instance Evaluatable Exp where
  eval (Const b) = eval b
  eval (And a b) = eval a && eval b
  eval (Or a b) = eval a || eval b
