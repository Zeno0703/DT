import Control.Monad
data ITree a = Ask String (Bool -> ITree a) | Result a | Tell String (ITree a)

examen :: ITree Int
examen = 
    Ask "Heet je Broos?"
    (\b -> if b then Result 20 else Ask "Vind je Prolog leuker dan Haskell?"
                                    (\b' -> if b' then Result 0 else Result 10))

iTalk :: ITree a -> [Bool] -> ([String], a)
iTalk (Result r) _ = ([], r)
iTalk (Ask q p) (x:xs) = iTalk (p x) xs
iTalk (Tell m t) as = let (ms, x) = iTalk t as
                        in (m:ms, x)

data Uitslag = Geslaagd | Gebuisd deriving Show

iMap :: (a -> b) -> ITree a -> ITree b
iMap f (Result r) = Result (f r)
iMap f (Ask q p) = Ask q (\ans -> iMap f (p ans))

examenUitslag :: ITree Uitslag
examenUitslag = iMap puntenNaarUitslag examen

milderExamen :: ITree Uitslag
milderExamen = iExtend mildeUitbreiding examenUitslag

mildeUitbreiding :: Uitslag -> ITree Uitslag
mildeUitbreiding Geslaagd = Result Geslaagd
mildeUitbreiding Gebuisd = 
    Ask "Heb je je best gedaan?" (\b -> if b then Result Geslaagd else Result Gebuisd)

iExtend :: (a -> ITree b) -> ITree a -> ITree b
iExtend f (Result r) = f r
iExtend f (Ask q p) = Ask q (\ans -> iExtend f (p ans))


puntenNaarUitslag :: Int -> Uitslag
puntenNaarUitslag p 
    | p >= 10 = Geslaagd
    | otherwise = Gebuisd


five = print 5

name = do   putStrLn "Handen omhoog"
            name <- getLine
            putStrLn name

summation = do  n <- getLine
                list <- replicateM (read n::Int) getLine
                listm <- return (map (\n -> read n::Int) list)
                print (sum listm)