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
spawnPokemon (Spawn p _ _) = p

spawnLocation :: Spawn -> Location
spawnLocation (Spawn _ l _) = l

spawnTime :: Spawn -> Time
spawnTime (Spawn _ _ t) = t

spawnDay :: Spawn -> WeekDay
spawnDay (Spawn _ _ (Time d _ _)) = d

spawnHours :: Spawn -> Int
spawnHours (Spawn _ _ (Time _ h _)) = h

spawnMinutes :: Spawn -> Int
spawnMinutes (Spawn _ _ (Time _ _ m)) = m

-- | 2. Group a list of `Spawn`s by a given function.
groupSpawnsBy :: Eq k => (Spawn -> k) -> [Spawn] -> [(k, [Spawn])]
groupSpawnsBy f l = [(key, getPokes key) | key <- distinctSpawns]
    where
        distinctSpawns = nub (map f l)
        getPokes p = filter (\s -> f s == p) l

topSpawn :: Eq k => (Spawn -> k) -> [Spawn] -> [(k, Int)]
topSpawn f spawns = reverse (sortOn snd (map (\(p, s) -> (p, length s)) (groupSpawnsBy f spawns)))

-- | 3. Which Pokémon spawns most often?
mostCommonPokemon :: [Spawn] -> [(Pokemon, Int)]
mostCommonPokemon = topSpawn spawnPokemon

-- | 4. At which spawn point does the given Pokémon spawn most often?
topSpawnPointsOf :: Pokemon -> [Spawn] -> [(Location, Int)]
topSpawnPointsOf name spawns = reverse (sortOn snd removed)
    where
        filtered = map (\(loc, list) -> (loc, filter (\(Spawn p _ _) -> p == name) list)) (groupSpawnsBy spawnLocation spawns)
        lengths = map (\(l, s) -> (l, length s)) filtered
        removed = filter (\(loc, amount) -> amount /= 0) lengths

-- | 5. During which hours do the most Pokémon spawn?
topHours :: [Spawn] -> [(Int, Int)]
topHours = topSpawn spawnHours

-- | 6. On which day of the week do the most Pokémon spawn?
topWeekDays :: [Spawn] -> [(WeekDay, Int)]
topWeekDays = topSpawn spawnDay

-- | 7. How many Pokémon spawn during the day, how many during the night?
dayAndNight :: [Spawn] -> (Int, Int)
dayAndNight spawns = (day, night)
    where
        day = length (filter (\(Spawn _ _ (Time _ h _)) -> h >= 7 && h <= 21) spawns)
        night = length (filter (\(Spawn _ _ (Time _ h _)) -> h < 7 || h > 21) spawns)

-- | 8. How many Pokémon spawn around the hour and how many between the hours?
aroundTheHours :: [Spawn] -> (Int, Int)
aroundTheHours spawns = (around, between)
    where
        between = length (filter (\(Spawn _ _ (Time _ _ h)) -> h > 15 && h < 45) spawns)
        around = length (filter (\(Spawn _ _ (Time _ _ h)) -> h <= 15 || h >= 45) spawns)

-- | 9. Analyse the spawn data.
analyseSpawns :: IO ()
analyseSpawns = undefined
