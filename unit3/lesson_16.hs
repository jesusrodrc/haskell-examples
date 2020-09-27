data BreakfastSide = Toast | Biscuit | Homefries | Fruit deriving Show
data BreakfastMeat = Sausage | Bacon | Ham deriving Show
data BreakfastMain = Egg | Pancake | Waffle deriving Show

data AuthorName = AuthorName {
    firstName :: String,
    lastName :: String
}

type FirstName = String
type LastName = String
type MiddleName = String

data Name = Name FirstName LastName 
    | NameWithMiddle FirstName MiddleName LastName
    | TwoInitialsWithLast Char Char LastName
    | FirstNameWithTwoInits FirstName Char Char LastName

data Creator = AuthorCreator Author | ArtistCreator Artist

data Author = Author Name

data Artist = Person Name | Band String

hpLovecraft :: Creator
hpLovecraft = AuthorCreator 
                (Author 
                    (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

data Book = Book {
    author :: Creator,
    isbn :: String,
    title :: String,
    year :: Int,
    bookPrice :: Double
}

data VinylRecord = VinylRecord {
    artist :: Creator,
    recordTitle :: String,
    recordYear :: Int,
    recordPrice :: Double
}

data CollectibleToy = CollectibleToy {
    name :: String,
    description :: String,
    toyPrice :: Double
}

data Pamphlet = Pamphlet {
    pamphletTitle :: String,
    pamphletDescription :: String,
    contact :: String
}

data StoreItem = BookItem Book 
    | RecordItem VinylRecord 
    | ToyItem CollectibleToy 
    | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (PamphletItem _) = 0.0


data Circle = Circle {
    radius :: Double
}

data Square = Square {
    sideLength :: Double
}

data Rectangle = Rectangle {
    height :: Double,
    width :: Double
}

data Shape = CircleShape Circle 
    | SquareShape Square
    | RectangularShape Rectangle

areaCalculation :: Shape -> Double
areaCalculation (CircleShape circle) = pi * (radius circle)^2
areaCalculation (SquareShape square) = (sideLength square) ^ 2
areaCalculation (RectangularShape rectangle) = (height rectangle) * (width rectangle)

perimeterCalculation :: Shape -> Double
perimeterCalculation (CircleShape circle) = 2 * pi * (radius circle)
perimeterCalculation (SquareShape square) = (sideLength square) * 4
perimeterCalculation (RectangularShape rectangle) = 2*(height rectangle)  + 2*(width rectangle)

