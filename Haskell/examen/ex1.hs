module MyHaskell where
import Data.List

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
spawnPokemon (Spawn name _ _) = name

spawnLocation :: Spawn -> Location
spawnLocation (Spawn _ (x, y) _) = (x, y)

spawnTime :: Spawn -> Time
spawnTime (Spawn _ _ (Time day h m)) = Time day h m

-- | 2. Group a list of `Spawn`s by a given function.
groupSpawnsBy :: Eq k => (Spawn -> k) -> [Spawn] -> [(k, [Spawn])]
groupSpawnsBy f spawns = [(key, getSpawns key) | key <- getUniqueKeys]
        where
            getUniqueKeys = nub $ map f spawns
            getSpawns key = filter (\spawn -> f spawn == key) spawns


-- | 3. Which Pokémon spawns most often?
mostCommonPokemon :: [Spawn] -> [(Pokemon, Int)]
mostCommonPokemon spawns = reverse (sortOn snd (map (\(key, list) -> (key, length list)) (groupSpawnsBy spawnPokemon spawns)))

-- | 4. At which spawn point does the given Pokémon spawn most often?
topSpawnPointsOf :: Pokemon -> [Spawn] -> [(Location, Int)]
topSpawnPointsOf name list =  reverse (sortOn snd (filter (\(_, len) -> len /= 0) (map (\(x, spawns) -> (x, length spawns)) filteredPokemans)))
                where
                    filteredPokemans = map (\(loc, pokes) -> (loc, filter (\(Spawn n _ _ ) -> n == name) pokes)) (groupSpawnsBy spawnLocation list)

-- | 5. During which hours do the most Pokémon spawn?
topHours :: [Spawn] -> [(Int, Int)]
topHours spawns = reverse (sortOn snd ([(hour, getPokes hour) | hour <- getUniqueHours]))
                where
                    getUniqueHours = nub $ map (\((Time _ h _), _) -> h) (groupSpawnsBy spawnTime spawns)
                    getPokes hour = length (filter (\((Time _ h _), _) -> h == hour) (groupSpawnsBy spawnTime spawns))

-- | 6. On which day of the week do the most Pokémon spawn?
topWeekDays :: [Spawn] -> [(WeekDay, Int)]
topWeekDays spawns = reverse (sortOn snd ([(day, getPokes day) | day <- getUniqueHours]))
                where
                    getUniqueHours = nub $ map (\((Time d _ _), _) -> d) (groupSpawnsBy spawnTime spawns)
                    getPokes day = length (filter (\((Time d _ _), _) -> d == day) (groupSpawnsBy spawnTime spawns))

-- | 7. How many Pokémon spawn during the day, how many during the night?
dayAndNight :: [Spawn] -> (Int, Int)
dayAndNight spawns = (day, night)
                where
                    day = length (filter (\((Time _ h _), _) -> h >= 7 && h <= 21) (groupSpawnsBy spawnTime spawns))
                    night = length (filter (\((Time _ h _), _)  -> h < 7 || h > 21) (groupSpawnsBy spawnTime spawns))

-- | 8. How many Pokémon spawn around the hour and how many between the hours?
aroundTheHours :: [Spawn] -> (Int, Int)
aroundTheHours spawns = (around, between)
                where
                    around = length (filter (\((Time _ _ m), _) -> m >= 45 || m <= 15) (groupSpawnsBy spawnTime spawns))
                    between = length (filter (\((Time _ _ m), _)  -> m < 45 && m > 15) (groupSpawnsBy spawnTime spawns))

-- | 9. Analyse the spawn data.
analyseSpawns :: IO ()
analyseSpawns = do
                    putStrLn "# Most common Pokemon:"
                    let common = mostCommonPokemon testSpawns
                    printCommon 1 common
                    putStrLn ""
                    putStrLn "# Top spawn points of Pidgey:"
                    let topSpawns = topSpawnPointsOf "Pidgey" testSpawns
                    printTopSpawnPoints "Pidgeys" 1 topSpawns
                    putStrLn ""
                    putStrLn "# Top hours:"
                    let hours = topHours testSpawns
                    printHours 1 hours
                    putStrLn ""
                    putStrLn "# Top week days:"
                    let weekdays = topWeekDays testSpawns
                    printWeekdays 1 weekdays
                    putStrLn ""
                    let dayNight = dayAndNight testSpawns
                    printDayVNight dayNight
                    putStrLn ""
                    let aroundBetween = aroundTheHours testSpawns
                    printAround aroundBetween

printCommon :: Int -> [(Pokemon, Int)] -> IO ()
printCommon _ [] = return ()
printCommon n (x:xs) = do
                        let (name, amount) = x
                        putStrLn (show n ++ ". " ++ name ++ " spawned " ++ show amount ++ " times")
                        printCommon (n + 1) xs

printTopSpawnPoints :: String -> Int -> [(Location, Int)] -> IO ()
printTopSpawnPoints _ _ [] = return ()
printTopSpawnPoints name n (x:xs) = do
                                let (loc, amount) = x
                                putStrLn (show n ++ ". at " ++ show loc ++ " " ++ show amount ++ " " ++ name ++ " spawned")
                                printTopSpawnPoints name (n + 1) xs

printHours :: Int -> [(Int, Int)] -> IO ()
printHours _ [] = return ()
printHours n (x:xs) = do
                        let (hour, amount) = x
                        putStrLn (show n ++ ". at " ++ show hour ++ " o'clock " ++ show amount ++ " Pokemon spawned ")
                        printHours (n + 1) xs

printWeekdays :: Int -> [(WeekDay, Int)] -> IO ()
printWeekdays _ [] = return ()
printWeekdays n (x:xs) = do
                        let (day, amount) = x
                        putStrLn (show n ++ ". on " ++ show day ++ " " ++ show amount ++ " Pokemon spawned")
                        printWeekdays (n + 1) xs 

printDayVNight :: (Int, Int) -> IO ()
printDayVNight (day, night) = do
                                let action  | day >= night = putStrLn ("More Pokemon spawn during the day than during the night: " ++ show day ++ " vs " ++ show night)
                                            | day < night = putStrLn ("More Pokemon spawn during the night than during the day: " ++ show day ++ " vs " ++ show night)
                                action

printAround :: (Int, Int) -> IO ()
printAround (around, between) = do
                                let action  | around >= between = putStrLn ("More Pokemon spawn around the hours than between the hours: " ++ show around ++ " vs " ++ show between)
                                            | around < between = putStrLn ("More Pokemon spawn between the hours than around the hours: " ++ show around ++ " vs " ++ show between)
                                action