
module Template where

-- * Arithmetic Expressions
-- ----------------------------------------------------------------------------

data Exp = Const Int
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
  deriving (Show, Eq)

eval :: Exp -> Int
eval (Const a) = a
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2

data Inst = IPush Int | IAdd | ISub | IMul
  deriving (Show, Eq)

type Prog  = [Inst]
type Stack = [Int]

runtimeError :: Stack
runtimeError = error "Runtime error."

execute :: Inst -> Stack -> Stack
execute (IPush i) s = i : s 
execute IAdd (x:y:xs) = (x + y) : xs
execute IAdd [x] = runtimeError
execute IAdd [] = runtimeError
execute ISub (x:y:xs) = (y - x) : xs
execute ISub [x] = runtimeError
execute ISub [] = runtimeError
execute IMul (x:y:xs) = (x * y) : xs
execute IMul [x] = runtimeError
execute IMul [] = runtimeError

run :: Prog -> Stack -> Stack
run instr s = foldl (\r x -> execute x r) s instr

compile :: Exp -> Prog
compile (Const a) = [IPush a]
compile (Add a b) = compile a ++ compile b ++ [IAdd]
compile (Sub a b) = compile a ++ compile b ++ [ISub]
compile (Mul a b) = compile a ++ compile b ++ [IMul]