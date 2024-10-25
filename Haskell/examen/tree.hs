module Template where

data ITree a
  = Ask String (Bool -> ITree a)
  | Result a
  | Tell String (ITree a)

burger_price :: ITree Float
burger_price =
  Ask "Do you want a big whopper?"
   (\ big -> if big 
               then -- big whopper
                  Ask "Do you want fries with that?"
                   (\ fries -> if fries
                                 then Result 8.5
                                 else Result 5.5 
                   )
               else -- small whopper
                  Ask "Do you want it in a happy kids box?"
                   (\ box -> if box
                                 then Result 10.0
                                 else Result 4.5 
                   )
   )

ieval0 :: ITree a -> [Bool] -> a
ieval0 (Result r) answers  =  r
ieval0 (Ask q ts) (b:as)   =  ieval0 (ts b) as

--------------------------------------------------------------------------------
-- * Assignment 1
--------------------------------------------------------------------------------

ieval1 :: ITree a -> [Bool] -> Maybe a
ieval1 (Result r) answers  = Just r
ieval1 (Ask q ts) [] = Nothing
ieval1 (Ask q ts) (b:as)   = ieval1 (ts b) as

--------------------------------------------------------------------------------
-- * Assignment 2
--------------------------------------------------------------------------------

ieval2 :: ITree a -> [Bool] -> ([String],Maybe a)
ieval2 (Result r) answers  = ([], Just r)
ieval2 (Tell q tree) (b:as) = (q:questions, r)
                    where (questions, r) = ieval2 tree as
ieval2 (Ask q ts) [] = ([q], Nothing) 
ieval2 (Ask q ts) (b:as) = (q:questions, r)
                    where (questions, r) = ieval2 (ts b) as

--------------------------------------------------------------------------------
-- * Assignment 3
--------------------------------------------------------------------------------

result :: a -> ITree a
result = Result

--------------------------------------------------------------------------------
-- * Assignment 4
--------------------------------------------------------------------------------

ask :: String -> ITree Bool
ask q = Ask q result

--------------------------------------------------------------------------------
-- * Assignment 5
--------------------------------------------------------------------------------

tell :: String -> ITree ()
tell m = Tell m (Result ())

--------------------------------------------------------------------------------
-- * Assignment 6
--------------------------------------------------------------------------------

extend :: ITree a -> (a -> ITree b) -> ITree b
extend (Result x) f = f x
extend (Tell q t) f = Tell q (extend t f)
extend (Ask q t)  f = Ask q (\ b -> extend (t b) f)

--------------------------------------------------------------------------------
-- * Assignment 7
--------------------------------------------------------------------------------

ieval7 :: ITree a -> IO a
ieval7 (Result r) = return r
ieval7 (Tell q t) = do  putStrLn q
                        ieval7 t
ieval7 (Ask q t) = do   putStrLn q
                        ans <- getLine
                        let action  | ans == "yes" = ieval7 (t True)
                                    | ans == "no" = ieval7 (t False)
                                    | otherwise = ieval7 (Ask q t)
                        action
