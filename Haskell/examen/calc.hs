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
repr (Num a) = show a
repr (Add a) = repr (Num a) ++ " +"
repr (Sub a) = repr (Num a) ++ " -"

isInt :: String -> Bool
isInt [] = True
isInt (x:xs) = ((x == '-' && not (null xs)) || isNumber x) && isNum xs
      where
          isNum [] = True
          isNum (x:xs) = isNumber x && isNum xs

transition :: State -> String -> State
transition Init num
                      | isInt num = Num (read num::Int)
                      | otherwise = Init
transition (Num num) op
                      | op == "+" = Add num
                      | op == "-" = Sub num
                      | isInt op = Num (read op::Int)
                      | op == "reset" = Init
                      | otherwise = Num num
transition (Add num) op
                      | isInt op = Num (num + (read op::Int))
                      | op == "-" = (Sub num)
                      | op == "reset" = Init
                      | otherwise = Add num
transition (Sub num) op
                      | isInt op = Num (num - (read op::Int))
                      | op == "+" = Add num
                      | op == "reset" = Init
                      | otherwise = Sub num

calculator :: IO ()
calculator = calculate Init

calculate :: State -> IO ()
calculate state = do
                input <- getLine
                if input == "exit" then putStrLn "Goodbye :)" else do   let new = transition state input
                                                                        putStrLn (repr new)
                                                                        calculate new