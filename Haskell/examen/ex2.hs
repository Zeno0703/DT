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

data Employee =   ICTe Int String Int Int | 
                  HRe Int String Int Int Int |
                  Cleaninge Int String Int Int
  deriving (Eq)

type EmployeeRecord = [Employee]

instance Show Employee where
  show (ICTe nr name wage hours) = (show nr) ++ " --- " ++ name ++ " --- " ++ (show wage) ++ " euro/month --- " ++ (show hours) ++ " hours"
  show (HRe nr name wage hours conflicts) = (show nr) ++ " --- " ++ name ++ " --- " ++ (show wage) ++ " euro/month --- " ++ (show hours) ++ " hours --- " ++ (show conflicts) ++ " conflicts" 
  show (Cleaninge nr name wage rooms) = (show nr) ++ " --- " ++ name ++ " --- " ++ (show wage) ++ " euro/month --- " ++ (show rooms) ++ " rooms"

createEmployee :: EmployeeInput -> Int -> Employee
createEmployee (name, ICT, wage) nr = ICTe nr name wage 0
createEmployee (name, HR, wage) nr = HRe nr name wage 0 0
createEmployee (name, Cleaning, wage) nr = Cleaninge nr name wage 0

createInitEmployees :: [EmployeeInput] -> EmployeeRecord
createInitEmployees = createList 1

createList :: Int -> [EmployeeInput] -> EmployeeRecord
createList _ [] = []
createList n (x:xs) = createEmployee x n : createList (n + 1) xs
 

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
getMatchingPercentage dept list (_, skills, _) = (skillLen * 100) `div` allSkillsLen
                  where
                    allSkills = getSkills dept list
                    matchingSkills = filter (`elem` allSkills) skills
                    skillLen = length matchingSkills
                    allSkillsLen = length allSkills

getSkills :: Department -> [Requirement] -> [String]
getSkills dept list = [skill | (d, skill) <- list, d == dept]

sortCandidates :: Department -> [Requirement] -> [Candidate] -> [Candidate]
sortCandidates dept reqs candidates = map fst sortedCand
                    where
                      allCandidatesWithPerc = generateCandidates dept reqs candidates
                      sortedCand = getOrdering allCandidatesWithPerc

generateCandidates :: Department -> [Requirement] -> [Candidate] -> [(Candidate, Int)]
generateCandidates _ _ [] = []
generateCandidates dept reqs (c:cs) = (c, getMatchingPercentage dept reqs c) : generateCandidates dept reqs cs

xxx ((_, _, wage1), perc1) ((_, _, wage2), perc2)   | perc1 < perc2 = LT
                                                    | perc1 == perc2 && wage1 > wage2 = LT
                                                    | otherwise = GT

getOrdering :: [(Candidate, Int)] -> [(Candidate, Int)]
getOrdering list = reverse (sortBy xxx list)

hireCandidate :: Department -> Int -> [Requirement] -> [Candidate] -> Maybe Candidate
hireCandidate dept maxWage reqs candidates
                                          | wage < maxWage = Just cand
                                          | otherwise = Nothing
                                          where
                                            cand = head (sortCandidates dept reqs candidates)
                                            (_, _, wage) = cand

executeHire :: Department -> Int -> [Requirement] -> [Candidate] -> EmployeeRecord -> EmployeeRecord
executeHire dept maxWage reqs candidates record =
  case hireCandidate dept maxWage reqs candidates of
    Just (name, skills, wage) -> record ++ [createEmployee (name, dept, wage) (length record + 1)]
    Nothing          -> record

-- ---------------------------------------------------------------------
-- MAIN
-- ---------------------------------------------------------------------

mainManager :: IO ()
mainManager = mainBase example_requirements example_candidates $ createInitEmployees example_employees

readDepartment :: String -> Maybe Department
readDepartment dept 
                    | dept == "ICT" = Just ICT
                    | dept == "HR" = Just HR
                    | dept == "Cleaning" = Just Cleaning
                    | otherwise = Nothing


prettyPrintEmployeeRecord :: EmployeeRecord -> IO ()
prettyPrintEmployeeRecord record = do
                                      putStrLn "+--------------------------------------------------"
                                      let action  | null record = putStrLn "| EMPTY"
                                                  | otherwise = printBase record
                                      action
                                      putStrLn "+--------------------------------------------------"

printBase :: EmployeeRecord -> IO ()
printBase [] = return ()
printBase (r:rs) = do
                      putStrLn ("| " ++ show r)
                      printBase rs

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

