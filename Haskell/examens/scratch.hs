import Data.Time

fib0 :: Int -> Int
fib0 0 = 0
fib0 1 = 1
fib0 n = fib0 (n - 1) + fib0 (n - 2)

fib1 :: Int -> [(Int, Int)]
fib1 0 = [(0, fib0 0)]
fib1 1 = [(1, fib0 1)]
fib1 n = (n, fib0 n) : fib1 (n - 1) ++ fib1 (n - 2)


data RoseTree a = RoseTree a [RoseTree a] deriving(Eq, Show)

ownMap f1 f2 l = map f1 (init l) ++ f2 (last l)

fib2 :: Int -> RoseTree Int
fib2 0 = RoseTree 0 []
fib2 1 = RoseTree 1 []
fib2 n = RoseTree n [fib2 (n-1), fib2 (n-2)]


timeFunction :: IO ()
timeFunction = do
    startTime <- getCurrentTime
    let ans = fib0 25
    print ans
    endTime <- getCurrentTime
    let diff = diffUTCTime endTime startTime
    print diff
    startTime <- getCurrentTime
    let ans = fib1 25
    print (ans !! 1)
    endTime <- getCurrentTime
    let diff = diffUTCTime endTime startTime
    print diff
    startTime <- getCurrentTime
    let ans = fib2 25
    print ans
    endTime <- getCurrentTime
    let diff = diffUTCTime endTime startTime
    print diff
