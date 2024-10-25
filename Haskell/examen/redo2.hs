module Personnel where

import Data.List
import Data.Maybe
import Text.Read

-- ---------------------------------------------------------------------
-- PERSONNEL
-- ---------------------------------------------------------------------

type EmployeeInput  = ( String     -- Name
                      , Department -- Department
                      , Int        -- Wage
                      )

data Department = ICT | HR | Cleaning
  deriving (Eq, Show)

data Employee = ICTe Int String Int Int |
                HRe Int String Int Int Int |
                Cleaninge Int String Int Int
  deriving (Eq)

type EmployeeRecord = [Employee]

instance Show Employee where
    show (ICTe id name wage hours) = show id ++ " --- " ++ name ++ " --- " ++ show wage ++ " euro/month --- " ++ show hours ++ " hours" 
    show (HRe id name wage hours conflicts) = show id ++ " --- " ++ name ++ " --- " ++ show wage ++ " euro/month --- " ++ show hours ++ " hours --- " ++ show conflicts ++ " conflicts" 
    show (Cleaninge id name wage rooms) = show id ++ " --- " ++ name ++ " --- " ++ show wage ++ " euro/month --- " ++ show rooms ++ " rooms" 

createEmployee :: EmployeeInput -> Int -> Employee
createEmployee (name, dept, wage) id
                                | dept == ICT = ICTe id name wage 0
                                | dept == HR = HRe id name wage 0 0
                                | otherwise = Cleaninge id name wage 0

createInitEmployees :: [EmployeeInput] -> EmployeeRecord
createInitEmployees list = createEmployeesWithID list 1

createEmployeesWithID :: [EmployeeInput] -> Int -> [Employee]
createEmployeesWithID [] _ = []
createEmployeesWithID (x:xs) n = createEmployee x n : createEmployeesWithID xs (n + 1)

-- ---------------------------------------------------------------------
-- HIRING
-- ---------------------------------------------------------------------

type Requirement = ( Department -- Department
                   , String     -- Required skill
                   )
type Candidate = ( String   -- Name
                 , [String] -- Skills
                 , Int      -- Wage
                 )

getMatchingPercentage :: Department -> [Requirement] -> Candidate -> Int
getMatchingPercentage dept requirements (_, reqs, _) = (amountCandidate * 100) `div` amountReq
            where
                deptReq = map snd (filter (\(d, r) -> d == dept) requirements)
                usefulSkills = filter (`elem` deptReq) reqs
                amountCandidate = length usefulSkills
                amountReq = length deptReq

sortCandidates :: Department -> [Requirement] -> [Candidate] -> [Candidate]
sortCandidates dept reqs list = map fst sortedCandidates
            where
                allCandidates = generateCandidates dept reqs list
                sortedCandidates = sortByPerc allCandidates


generateCandidates :: Department -> [Requirement] -> [Candidate] -> [(Candidate, Int)]
generateCandidates _ _ [] = []
generateCandidates dept reqs (x:xs) = (x, getMatchingPercentage dept reqs x) : generateCandidates dept reqs xs

sortByPerc :: [(Candidate, Int)] -> [(Candidate, Int)]
sortByPerc = sortBy perc

perc ((_, _, wage1), perc1) ((_, _, wage2), perc2)  | perc1 < perc2 = GT
                                                    | perc1 == perc2 && wage1 > wage2 = GT
                                                    | otherwise = LT

hireCandidate :: Department -> Int -> [Requirement] -> [Candidate] -> Maybe Candidate
hireCandidate dept maxWage reqs cands
                    | wage <= maxWage = Just (head sortedCands)
                    | otherwise = Nothing
                where
                    sortedCands = sortCandidates dept reqs cands
                    (_, _, wage) = head sortedCands

executeHire :: Department -> Int -> [Requirement] -> [Candidate] -> EmployeeRecord -> EmployeeRecord
executeHire dept maxWage reqs cands record = 
    case hireCandidate dept maxWage reqs cands of
        Just (name, skills, wage) -> record ++ [createEmployee (name, dept, wage) (length record + 1)]
        Nothing -> record

-- ---------------------------------------------------------------------
-- MAIN
-- ---------------------------------------------------------------------

mainManager :: IO ()
mainManager = mainBase example_requirements example_candidates $ createInitEmployees example_employees

readDepartment :: String -> Maybe Department
readDepartment s 
                | s == "ICT" = Just ICT
                | s == "HR" = Just HR
                | s == "Cleaning" = Just Cleaning 
                | otherwise = Nothing

prettyPrintEmployeeRecord :: EmployeeRecord -> IO ()
prettyPrintEmployeeRecord record = do
                                putStrLn "+--------------------------------------------------"
                                let action  | null record = putStrLn "| EMPTY"
                                            | otherwise = loopAll record
                                action
                                putStrLn "+--------------------------------------------------"

loopAll :: EmployeeRecord -> IO ()
loopAll [] = return ()
loopAll (x:xs) = do
                    putStrLn ("| " ++ show x)
                    loopAll xs 

mainBase :: [Requirement] -> [Candidate] -> EmployeeRecord -> IO ()
mainBase reqs cands record = do
                                putStrLn "Welcome to the Personnel Register"
                                putStrLn "Enter the department where to hire:"
                                department <- getLine
                                let dept = readDepartment department
                                let action  | isNothing dept = do putStrLn "Please try again"
                                                                  putStrLn ""
                                                                  mainBase reqs cands record
                                            | otherwise = do  putStrLn "Enter the budget"
                                                              wage <- getLine
                                                              putStrLn "Personnel register"
                                                              let reg = executeHire (fromJust dept) (read wage::Int) reqs cands record
                                                              prettyPrintEmployeeRecord reg
                                action
                                
-- ---------------------------------------------------------------------
-- EXAMPLE DATA
-- ---------------------------------------------------------------------

example_employees :: [EmployeeInput]
example_employees = [ ("Tony",  ICT,      5000)
                    , ("Bruce", ICT,      2000)
                    , ("Nick",  HR,       2000)
                    , ("Phil",  HR,       1500)
                    , ("Steve", Cleaning, 1500)
                    ]

example_requirements :: [Requirement]
example_requirements = [ (ICT,      "Haskell")
                       , (ICT,      "Prolog")
                       , (ICT,      "Git")
                       , (HR,       "PeopleSkills")
                       , (HR,       "Connections")
                       , (Cleaning, "Experience")
                       , (Cleaning, "Motivation")
                       ]

example_candidates :: [Candidate]
example_candidates = [ ("Peter",    ["Haskell", "Git", "Motivation"],                                   1000)
                     , ("Ben",      ["Haskell", "PeopleSkills", "Connections", "Experience", "Wisdom"], 5000)
                     , ("May",      ["PeopleSkills", "Experience", "Motivation"],                       2000)
                     , ("MaryJane", ["Prolog", "Connections", "Looks"],                                 1500)
                     , ("Harry",    ["Connections", "Motivation", "Money"],                             8000)
                     ]

