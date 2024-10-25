module Template where

game :: IO ()
game = do   putStrLn "Think of a number between 1 and 100!"
            guess 1 100

guess :: Int -> Int -> IO ()
guess a b = do  let mid =  div (a + b)  2
                putStr ("Is it " ++ show mid ++ "? ")
                input <- getLine
                let action  | input == "higher" = guess (mid + 1) b
                            | input == "lower" = guess a (mid - 1)
                            | input == "yes" = putStrLn "Great, I won!"
                            | otherwise = putStrLn "Unrecognized input, please start over."
                action



game2 :: Int -> IO ()
game2 n = do    putStr "What is your guess? "
                guess2 n

guess2 :: Int -> IO ()
guess2 n = do   guess <- getLine
                let action  | (read guess::Int) > n = do    putStrLn "lower"
                                                            guess2 n
                            | (read guess::Int) < n = do    putStrLn "higher"
                                                            guess2 n
                            | otherwise = putStrLn "Congratulations, you have finally, after many attempts, guessed my number."
                action