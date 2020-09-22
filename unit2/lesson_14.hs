data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Show)

instance Show SixSidedDie where
    show S1 = "one"
    show S2 = "two"
    show S3 = "three"
    show S4 = "four"
    show S5 = "five"
    show S6 = "six"

data FiveSidedDie = S1 | S2 | S3 | S4 | S5 deriving (Enum, Eq, Show)

class (Eq a, Enum a) => Die a where
    roll :: Int -> a

instance Die FiveSidedDie where
    roll n = toEnum (n 'mod' 5)