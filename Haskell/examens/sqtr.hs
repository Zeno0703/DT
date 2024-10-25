module Template where

import Data.Char (chr)
import Control.Monad

-- * Rectangles and Squares
-- ----------------------------------------------------------------------------

rectangle :: Int -> Int -> IO ()
rectangle w h = replicateM_ h (putStrLn (replicate w '*')) 

square :: Int -> IO ()
square dimension = rectangle dimension dimension

-- * Trapezoids and Triangles
-- ----------------------------------------------------------------------------


trapezoid :: Int -> Int -> IO ()
trapezoid w 0 = return ()
trapezoid w h = do
                    putStrLn (replicate (h - 1) ' ' ++ replicate w '*')
                    trapezoid (w + 2) (h - 1) 

triangle :: Int -> IO ()
triangle = trapezoid 1
