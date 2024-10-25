module Template where

import Control.Monad

-- * Drilling on IO
-- ----------------------------------------------------------------------------

prog1 :: IO ()
prog1 = do 
            m <- getLine
            n <- getLine
            replicateM_ (read m::Int) (putStrLn n)

prog1b :: IO ()
prog1b = getLine >>= (\m -> getLine >>= (\n -> replicateM_ (read m::Int) (putStrLn n)))


prog2 :: IO ()
prog2 = do
            word <- getLine
            if word == "" then return() else do print (reverse word)
                                                prog2

index :: [IO a] -> IO Int -> IO a
index list indexIO = do
                        i <- indexIO
                        list !! i