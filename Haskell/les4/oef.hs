module Template where

import Control.Monad

-- * Drilling on IO
-- ----------------------------------------------------------------------------

prog1 :: IO ()
prog1 = do
            m <- getLine
            n <- getLine
            mapM_ putStrLn [n | l <- [1..(read m::Int)]]

prog1b :: IO ()
prog1b = getLine >>= \m ->
            getLine >>= \n ->
                mapM_ putStrLn [n | l <- [1..(read m::Int)]]

prog2 :: IO ()
prog2 = do
            input <- getLine
            if null input then return () else do    print (reverse input)
                                                    prog2

prog2b :: IO()
prog2b = do 
            input <- getLine
            when (not $ null input) $ do    print $ reverse input
                                            prog2b

index :: [IO a] -> IO Int -> IO a
index actions indexIO = indexIO >>= (actions !!)