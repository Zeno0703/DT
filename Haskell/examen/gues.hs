module Template where

game :: IO ()
game = do   
            putStrLn "Think of a number between 1 and 100!"
            gameHelper 1 100

gameHelper :: Int -> Int -> IO()
gameHelper x1 x2 = do
                        let middle = div (x1 + x2) 2
                        putStr ("Is it " ++ show middle ++ "? ")
                        answer <- getLine
                        let action  | answer == "higher" = gameHelper (middle + 1) x2
                                    | answer == "lower" = gameHelper x1 (middle - 1)
                                    | answer == "yes" = putStrLn "Great, I won!"
                                    | otherwise = putStrLn "Unrecognized input, please start over."
                        action

game2 :: Int -> IO ()
game2 x = do
            putStr "What is your guess? "
            game2Helper x

game2Helper :: Int -> IO ()
game2Helper x = do
                    input <- getLine
                    let action  | (read input::Int) < x = do    putStrLn "higher"
                                                                game2Helper x
                                | (read input::Int) > x = do    putStrLn "lower"
                                                                game2Helper x
                                | (read input::Int) == x = putStrLn "Congratulations, you have finally, after many attempts, guessed my number."
                    action