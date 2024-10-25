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
  show (ICTe id name wage hrs) = show id ++ " --- " ++ name ++ " --- " ++ show wage ++ " euro/month --- " ++ show hrs ++ " hours"
  show (HRe id name wage hrs conf) = show id ++ " --- " ++ name ++ " --- " ++ show wage ++ " euro/month --- " ++ show hrs ++ " hours ---" ++ show conf ++ " conflicts"
  show (Cleaninge id name wage rooms) = show id ++ " --- " ++ name ++ " --- " ++ show wage ++ " euro/month --- " ++ show rooms ++ " rooms"


createEmployee :: EmployeeInput -> Int -> Employee
createEmployee (name, ICT, wage) id = ICTe id name wage 0
createEmployee (name, HR, wage) id = HRe id name wage 0 0
createEmployee (name, Cleaning, wage) id = Cleaninge id name wage 0


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
getMatchingPercentage = error "not implemented"

sortCandidates :: Department -> [Requirement] -> [Candidate] -> [Candidate]
sortCandidates = error "not implemented"

hireCandidate :: Department -> Int -> [Requirement] -> [Candidate] -> Maybe Candidate
hireCandidate = error "not implemented"

executeHire :: Department -> Int -> [Requirement] -> [Candidate] -> EmployeeRecord -> EmployeeRecord
executeHire = error "not implemented"


-- ---------------------------------------------------------------------
-- MAIN
-- ---------------------------------------------------------------------

mainManager :: IO ()
mainManager = mainBase example_requirements example_candidates $ createInitEmployees example_employees

readDepartment :: String -> Maybe Department
readDepartment = error "not implemented"

prettyPrintEmployeeRecord :: EmployeeRecord -> IO ()
prettyPrintEmployeeRecord = error "not implemented"

mainBase :: [Requirement] -> [Candidate] -> EmployeeRecord -> IO ()
mainBase = error "not implemented"


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

