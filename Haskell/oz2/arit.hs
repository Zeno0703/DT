
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
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mul a b) = eval a * eval b

data Inst = IPush Int | IAdd | ISub | IMul
  deriving (Show, Eq)

type Prog  = [Inst]
type Stack = [Int]

runtimeError :: Stack
runtimeError = error "Runtime error."

execute :: Inst -> Stack -> Stack
execute (IPush a) s = a : s
execute IAdd (x:y:xs) = x + y: xs 
execute IAdd [] = runtimeError
execute IAdd [x] = runtimeError
execute ISub (x:y:xs) = y - x: xs
execute ISub [] = runtimeError
execute ISub [x] = runtimeError
execute IMul (x:y:xs) = x * y: xs 
execute IMul [] = runtimeError
execute IMul [x] = runtimeError

run :: Prog -> Stack -> Stack
run instr s = foldl (\r x -> execute x r) s instr

compile :: Exp -> Prog
compile (Const a) = [IPush a]
compile (Add a b) = (compile a) ++ (compile b) ++ [IAdd]
compile (Sub a b) = (compile a) ++ (compile b) ++ [ISub]
compile (Mul a b) = (compile a) ++ (compile b) ++ [IMul]

