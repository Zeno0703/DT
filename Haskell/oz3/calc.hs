module Template where

import Data.Char

data State =
      Init
    | Num Int
    | Add Int
    | Sub Int
    deriving (Eq, Show)

repr :: State -> String
repr Init = ""
repr (Num n) = show n
repr (Add n) = repr (Num n) ++ " +"
repr (Sub n) = repr (Num n) ++ " -"

isInt :: String -> Bool
isInt "" = False
isInt (x:xs) = ((x == '-' && not (null xs)) || isNumber x) && isNum xs
      where
        isNum [] = True
        isNum (x:xs) = isNumber x && isNum xs

transition :: State -> String -> State
transition Init s
                  | isInt s = Num (read s::Int)
                  | otherwise = Init
transition (Num n) s 
                  | s == "+" = Add n
                  | s == "-" = Sub n
                  | isInt s = Num (read s::Int)
                  | s == "reset" = Init
                  | otherwise = Num n
transition (Add n) s
                  | isInt s = Num (n + (read s::Int))
                  | s == "-" = Sub n
                  | s == "reset" = Init
                  | otherwise = Add n
transition (Sub n) s
                  | isInt s = Num (n - (read s::Int))
                  | s == "+" = Add n
                  | s == "reset" = Init
                  | otherwise = Sub n

calculator :: IO ()
calculator = calculate Init

calculate :: State -> IO ()
calculate state = do  s <- getLine
                      if s == "exit" then putStrLn "Goodbye :)" else do   let new = transition state s
                                                                          putStrLn (repr new)
                                                                          calculate new
