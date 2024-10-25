module Template where

game :: IO ()
game = do
            putStrLn "Think of a number between 1 and 100!"
            gameHelper 1 100

gameHelper :: Int -> Int -> IO ()
gameHelper low high = do
                            let middle = (low + high) `div` 2
                            putStr ("Is it " ++ show middle ++ "? ")
                            ans <- getLine
                            let action  | ans == "higher" = gameHelper (middle + 1) high 
                                        | ans == "lower" = gameHelper low (middle - 1)
                                        | ans == "yes" = putStrLn "Great, I won!"
                                        | otherwise = putStrLn "Unrecognized input, please start over."
                            action

game2 :: Int -> IO ()
game2 n = do
                putStr "What is your guess? "
                game2Helper n

game2Helper :: Int -> IO ()
game2Helper n = do
                    input <- getLine
                    let action  | (read input::Int) > n = do
                                                                putStrLn "lower"
                                                                game2Helper n
                                | (read input::Int) < n = do
                                                                putStrLn "higher"
                                                                game2Helper n
                                | otherwise = putStrLn "Congratz :)"
                    action
