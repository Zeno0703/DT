module Template where

import Data.Char (chr)
import Control.Monad
import GHC.IO.Handle (hGetChar)

-- * Rectangles and Squares
-- ----------------------------------------------------------------------------

rectangle :: Int -> Int -> IO ()
rectangle n m = replicateM_ m (putStrLn (replicate n '*'))

square :: Int -> IO ()
square dimension = rectangle dimension dimension

-- * Trapezoids and Triangles
-- ----------------------------------------------------------------------------

-- empties INT2 - 1 spaces voordien

trapezoid :: Int -> Int -> IO ()
trapezoid w 0 = return ()
trapezoid w h = do  putStrLn (replicate (h - 1) ' ' ++ replicate w '*')
                    trapezoid (w + 2) (h - 1)




triangle :: Int -> IO ()
triangle n = trapezoid 1 n
