
module Template where

data Name = N String
    deriving (Show)

data Pair = P (Int, Int)
    deriving (Show)

data Gender = Male | Female | Other
    deriving (Show)

data Person = MkPer Name Int Gender
    deriving (Show)

data TestResult = Pass Int | Fail [String]
    deriving (Show)


stringToGender :: String -> Gender
stringToGender g 
    | g == "Male" = Male
    | g == "Female" = Female
    | otherwise = Other

genderToString :: Gender -> String
genderToString = show


passing :: Int -> TestResult
passing grade = Pass grade  

failing :: [String] -> TestResult
failing message = Fail message

grade :: TestResult -> Int
grade (Pass grade) = grade
grade (Fail _) = 0

comments :: TestResult -> [String]
comments (Fail message) = message
comments (Pass _) = []