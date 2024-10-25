
module Template where

data Name = MkName String
    deriving (Show)

data Pair = MkPair Int Int
    deriving (Show)

data Gender = Male | Female | Other
    deriving (Show)

data Person = MkPerson Name Int Gender
    deriving (Show)

data TestResult = Pass Int | Fail [String]
    deriving (Show)


stringToGender :: String -> Gender
stringToGender s
    | s == "Male" = Male
    | s == "Female" = Female
    | otherwise = Other

genderToString :: Gender -> String
genderToString = show

passing :: Int -> TestResult
passing = Pass

failing :: [String] -> TestResult
failing = Fail

grade :: TestResult -> Int
grade (Pass x) = x
grade (Fail _) = 0

comments :: TestResult -> [String]
comments (Pass _) = []
comments (Fail messages) = messages