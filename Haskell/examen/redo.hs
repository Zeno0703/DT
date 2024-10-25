module MyHaskell where
import Data.List (sortOn)


-- | A Pokémon is represented by its name.
--
-- Don't worry, when you're writing tests, you don't have to come up with
-- actual Pokémon names, but here are some in case you are uninspired:
-- Pikachu, Charmander, Squirtle, Bulbasaur
type Pokemon = String

-- | A location is a tuple of the latitude and longitude.
type Location = (Float, Float)

data WeekDay
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    deriving (Eq, Ord, Show, Read, Enum)

data Time = Time
            WeekDay
            Int      -- ^ Hours go from 0 to 23
            Int      -- ^ Minutes go from 0 to 59
            deriving (Eq, Ord, Show, Read)

-- | A Pokémon spawned at a location and time.
data Spawn = Spawn Pokemon Location Time
             deriving (Eq, Show, Read)


-- Toy data for the examples
pidgeySpawn1 :: Spawn
pidgeySpawn1 = Spawn "Pidgey" (50.86296,4.674903) (Time Tuesday 12 04)

pidgeySpawn2 :: Spawn
pidgeySpawn2 = Spawn "Pidgey" (50.864605,4.6786203) (Time Friday 3 32)

pikachuSpawn :: Spawn
pikachuSpawn = Spawn "Pikachu" (50.864605,4.6786203) (Time Friday 12 04)

testSpawns :: [Spawn]
testSpawns =  [pidgeySpawn1, pikachuSpawn, pidgeySpawn2]


-- | 1. Projection functions
spawnPokemon :: Spawn -> Pokemon
spawnPokemon (Spawn p _ _) = p

spawnLocation :: Spawn -> Location
spawnLocation (Spawn _ l _) = l

spawnTime :: Spawn -> Time
spawnTime (Spawn _ _ t) = t


spawnDay :: Spawn -> WeekDay
spawnDay (Spawn _ _ (Time d _ _)) = d

spawnHour :: Spawn -> Int
spawnHour (Spawn _ _ (Time _ h _)) = h

spawnMinute :: Spawn -> Int
spawnMinute (Spawn _ _ (Time _ _ m)) = m

-- | 2. Group a list of `Spawn`s by a given function.
groupSpawnsBy :: Eq k => (Spawn -> k) -> [Spawn] -> [(k, [Spawn])]
groupSpawnsBy f list = [(key, getSpawns key) | key <- distinctSpawns]
    where
        distinctSpawns = reverse (foldr (\x r -> if x `elem` r then r else x:r) [] [ f spawn | spawn <- list ])
        getSpawns poke = filter (\s -> f s == poke) list

-- | 3. Which Pokémon spawns most often?

topList :: Eq k => (Spawn -> k) -> [Spawn] -> [(k, Int)]
topList f list = reverse (sortOn snd [(fst all, length(snd all)) | all <- groupSpawnsBy f list])

mostCommonPokemon :: [Spawn] -> [(Pokemon, Int)]
mostCommonPokemon = topList spawnPokemon

-- | 4. At which spawn point does the given Pokémon spawn most often?
topSpawnPointsOf :: Pokemon -> [Spawn] -> [(Location, Int)]
topSpawnPointsOf p list = reverse (sortOn snd (filter (\t -> snd t /= 0) [(fst all, length (filter (\s -> spawnPokemon s == p) (snd all))) | all <- groupSpawnsBy spawnLocation list]))

-- | 5. During which hours do the most Pokémon spawn?
topHours :: [Spawn] -> [(Int, Int)]
topHours = topList spawnHour

-- | 6. On which day of the week do the most Pokémon spawn?
topWeekDays :: [Spawn] -> [(WeekDay, Int)]
topWeekDays = topList spawnDay

-- | 7. How many Pokémon spawn during the day, how many during the night?
tupleAmount :: (Spawn -> Int) -> [Spawn] -> Int -> Int -> (Int, Int)
tupleAmount f list a b = (amount1, amount2)
    where
        count = [f spawn | spawn <- list]
        amount1 = length (filter (\x -> x >= a && x <= b) count)
        amount2 = length (filter (\x -> x < a || x > b) count)

dayAndNight :: [Spawn] -> (Int, Int)
dayAndNight list = tupleAmount spawnHour list 7 21

-- | 8. How many Pokémon spawn around the hour and how many between the hours?
aroundTheHours :: [Spawn] -> (Int, Int)
aroundTheHours list = (amount1, amount2)
    where
        count = [spawnMinute spawn | spawn <- list]
        amount2 = length (filter (\x -> x >= 15 && x <= 45) count)
        amount1 = length (filter (\x -> x < 15 || x > 45) count)

-- | 9. Analyse the spawn data.
analyseSpawns :: IO ()
analyseSpawns = do
                    putStrLn "# Most common Pokemon:"
                    printMostCommon 1 (mostCommonPokemon testSpawns)
                    putStrLn ""
                    putStrLn "# Top spawn points of Pidgey"
                    printTopSpawnPoints "Pidgey" 1 (topSpawnPointsOf "Pidgey" testSpawns)
                    putStrLn ""
                    putStrLn "# Top Hours"
                    printHours 1 (topHours testSpawns)
                    putStrLn ""
                    putStrLn "# Top Week days"
                    printDays 1 (topWeekDays testSpawns)
                    putStrLn ""
                    printDayNight (dayAndNight testSpawns)
                    printAroundBetween (aroundTheHours testSpawns)


printMostCommon :: Int -> [(Pokemon, Int)] -> IO ()
printMostCommon _ [] = return ()
printMostCommon n (x:xs) = do
                                putStrLn (show n ++ ". " ++ fst x ++ " spawned " ++ show (snd x) ++ " times")
                                printMostCommon (n + 1) xs

printTopSpawnPoints :: String -> Int -> [(Location, Int)] -> IO ()
printTopSpawnPoints _ _ [] = return ()
printTopSpawnPoints p n (x:xs) = do
                                    putStrLn (show n ++ ". " ++ show (fst x) ++ " " ++ show (snd x) ++ " " ++ p ++ " spawned")
                                    printTopSpawnPoints p (n + 1) xs

printHours :: Int -> [(Int, Int)] -> IO ()
printHours _ [] = return ()
printHours n (x:xs) = do
                            putStrLn (show n ++ ". at " ++ show (fst x) ++ " o'clock " ++ show (snd x) ++ " Pokemon spawned")
                            printHours (n + 1) xs

printDays :: Int -> [(WeekDay, Int)] -> IO ()
printDays _ [] = return ()
printDays n (x:xs) = do
                        putStrLn (show n ++ ". on " ++ show (fst x) ++ show (snd x) ++ " Pokemon spawned")
                        printDays (n + 1) xs

printDayNight :: (Int, Int) -> IO ()
printDayNight (a, b)
                    | a > b = putStrLn ("More Pokemon spawn during the day than during the night: " ++ show a ++ " vs " ++ show b)
                    | otherwise = putStrLn ("More Pokemon spawn during the night than during the day: " ++ show a ++ " vs " ++ show b)

printAroundBetween :: (Int, Int) -> IO ()
printAroundBetween (a, b)
                    | a > b = putStrLn ("More Pokemon spawn around the hours than between the hours: " ++ show a ++ " vs " ++ show b)
                    | otherwise = putStrLn ("More Pokemon spawn between the hours than around the hours: " ++ show a ++ " vs " ++ show b)