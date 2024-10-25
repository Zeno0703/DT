module Myhaskell where

-------------------------------------------------------------------------------
-- PART I: Expense & Delta

-- IMPORTANT: MAKE SURE THE AMOUNT COMES BEFORE THE NAME
data Expense = MkExp Double String deriving (Eq,Ord)

mkExpense :: String -> Double -> Expense
mkExpense name amount = MkExp amount name

instance Show Expense where
  show (MkExp amount name) = name ++ ": " ++ show amount

data Delta = MkDelta Expense deriving (Eq,Ord)

instance Show Delta where
  show (MkDelta (MkExp amount name)) = name ++": " ++ show amount

fromExpense :: Double -> Expense -> Delta
fromExpense avgAmount (MkExp amount name) = MkDelta (MkExp (amount - avgAmount) name)

mkDelta :: String -> Double -> Delta
mkDelta name amount = fromExpense 0 (mkExpense name amount)

-- | Convert a list of Expenses to a list of Deltas
-- The deltas are with respect to the average expense.
toDeltas :: [Expense] -> [Delta]
toDeltas exp = map (fromExpense avg) exp
                where
                    amounts = map (\(MkExp amount name) -> amount) exp
                    avg = sum amounts / fromIntegral (length amounts) 

-------------------------------------------------------------------------------
-- PART II: Transferable Transfers

-- | The Transfer datatype: a money transfer from one person to another.
data Transfer = MkTransfer String String Double deriving Eq

trans_from :: Transfer -> String
trans_from (MkTransfer from _ _) = from

trans_to :: Transfer -> String
trans_to (MkTransfer _ to _) = to

trans_amount :: Transfer -> Double
trans_amount (MkTransfer _ _ amount) = amount

instance Show Transfer where
  show (MkTransfer from to amount) = from ++ " -> " ++ to ++ ":" ++ show amount

-- | The Transferable class contains types t to which a transfer can be applied,
-- and that can create a Transfer from one t to another t
class Transferable t where
  applyTransfer  :: Transfer -> t -> t

-- | Apply a list of Transfers to to a Transferable from left to right.
applyTransfers :: Transferable t => [Transfer] -> t -> t
applyTransfers transfers x = foldl (flip applyTransfer) x transfers

instance Transferable Expense where
  applyTransfer (MkTransfer p1 p2 amount) (MkExp amount2 p3)
                                                            | p1 == p2 = MkExp amount2 p3
                                                            | p3 == p1 = MkExp (amount + amount2) p1
                                                            | p3 == p2 = MkExp (amount2 - amount) p2
                                                            | otherwise = MkExp amount2 p3

instance Transferable Delta where
  applyTransfer (MkTransfer p1 p2 amount) (MkDelta (MkExp amount2 p3))
                                                            | p1 == p2 = MkDelta (MkExp amount2 p3)
                                                            | p3 == p1 = MkDelta (MkExp (amount + amount2) p1)
                                                            | p3 == p2 = MkDelta (MkExp (amount2 - amount) p2)
                                                            | otherwise = MkDelta (MkExp amount2 p3)
  -- IMPLEMENTATION REQUIRED

createTransfer :: Double -> Delta -> Delta -> Transfer
createTransfer amount (MkDelta (MkExp amount1 p1)) (MkDelta (MkExp amount2 p2)) = MkTransfer p1 p2 amount

-------------------------------------------------------------------------------
-- PART III: Balancing Expenses

-- | Check if a list of expenses is epsilon-balanced.
balanced :: [Expense] -> Double -> Bool
balanced = bal

bal :: [Expense] -> Double -> Bool
bal [] _ = True
bal (x:xs) e = all (==True) boolList && bal xs e
            where
                MkExp am _ = x
                boolList = map (\(a1, a2) -> abs(a1 - a2) < e) [(am, a) | (MkExp a _) <- xs]


-- | Epsilon-balance a list of Deltas.
balanceDeltas :: [Delta] -> Double -> [Transfer]
balanceDeltas = error "balanceDeltas: not yet implemented."

-- | Epsilon-balance a list of Expenses.
balance :: [Expense] -> Double -> [Transfer]
balance = error "balance: not yet implemented."


-------------------------------------------------------------------------------
-- PART IV: Application

-- | Read a list of expenses.
-- If the entered amount is non-negative, the list is terminated.
getExpenses :: IO [Expense]
getExpenses = error "getExpenses: not yet implemented."

-- | Print a list of transfers, each on a separate line 
printTransfers :: [Transfer] -> IO ()
printTransfers = error "printTransfers: not yet implemented."

-- | Read a list of Expenses, balance them, and print the required transfers.
balanceIO :: IO ()
balanceIO = error "balanceIO: not yet implemented."

{- Example:

> balanceIO
Name: Alex
Amount: 200
Name: Tom
Amount: 1000
Name: Thomas
Amount: 275.5
Name: 
Amount: 0
Gert-Jan -> Tom:338.875
Alex -> Tom:178.875
Thomas -> Tom:103.375

-}
