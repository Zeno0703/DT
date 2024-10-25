data Trie a = Node Bool [(a, Trie a)]

leaf :: Trie a
leaf = Node True []

accept :: [(a,Trie a)] -> Trie a
accept = Node True

reject :: [(a,Trie a)] -> Trie a
reject = Node False

mytrie :: Trie Int
mytrie = reject [(1, accept [(5, leaf)
                            ,(7, leaf)])
                            ,(3, leaf)
                ,(7, reject [(1, leaf)])]

height :: Trie a -> Int
height (Node _ []) = 1
height (Node _ ts) = 1 + maximum [height t | (_, t) <- ts]

hasPath :: Eq a => [a] -> Trie a -> Bool
hasPath [] (Node b _) = b
hasPath (x:xs) (Node b ts) = 
    case lookup x ts of
        Nothing -> False
        Just t -> hasPath xs t

instance Show a => Show (Trie a) where
    show = unlines . prettyPrint

prettyPrint :: Show a => Trie a -> [String]
prettyPrint (Node b []) = [if b then "x" else "o"]
prettyprint (Node b ts) = l1 : concat [addLabel x (prettyPrint t) | (x,t) <- rest] 
                                ++ addLabelLast lastTree
    where
        l1 = if b then 'x' else 'o'
        rest = init ts
        lastTree = last ts
        addLabel x (l:ls) = ("|-" ++ show x ++ "- " ++ l) : map ("|    " ++) ls
        addLabelLast (x,t) = ("`- " ++ show x ++ " -" ++ l) : map ("    " ++) ls
            where
                (l:ls) = prettyPrint t
