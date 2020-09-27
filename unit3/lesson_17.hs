import Data.List
import Data.Semigroup

myAny :: (a -> Bool) -> [a] -> Bool
myAny testFunc = (foldr (||) False) . (map testFunc)

instance Semigroup Integer where
    (<>) x y = x + y

data Color = Red 
        | Yellow 
        | Blue 
        | Green 
        | Purple 
        | Orange 
        | Brown 
        | Transparent deriving (Show, Eq)

instance Semigroup Color where
    (<>) Transparent a = a
    (<>) a Transparent = a
    (<>) Red Blue = Purple
    (<>) Blue Red = Purple
    (<>) Yellow Blue = Green
    (<>) Blue Yellow = Green
    (<>) Yellow Red = Orange
    (<>) Red Yellow = Orange
    (<>) a b | a == b = a
             | all (`elem` [Red, Blue, Purple]) [a,b] = Purple
             | all (`elem` [Blue, Yellow, Green]) [a,b] = Green
             | all (`elem` [Red, Yellow, Orange]) [a,b] = Orange
             | otherwise = Brown

instance Monoid Color where
    mempty = Transparent
    mappend colorA colorB = colorA <> colorB


type Events = Events [String]
type Probs = Probs [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
    where totalProbs = sum probs
          normalizedProbs = map (\x -> x/totalProbs) probs

showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
    show (PTable events probs) = mconcat pairs
        where pairs = zipWith showPair events probs

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func list1 list2 = zipWith func newL1 cycledL2
    where nToAdd = length list2
          repeatedL1 = map (take nToAdd . repeat) list1
          newL1 = mconcat repeatedL1
          cycledL2 = cycle list2

combineEvents :: Events -> Events -> Events
combineEvents (Events e1) (Events e2) = Events (cartCombine combiner e1 e2)
    where combiner = (\x y -> mconcat [x,"-",y])

instance Semigroup Events where
    (<>) = combineEvents

instance Monoid Events where
    mappend = (<>)
    mempty = Events []

combineProbs :: Probs -> Probs -> Probs
combineProbs (Probs p1) (Probs p2) = Probs (cartCombine (*) p1 p2)

instance Semigroup Probs where
    (<>) = combineProbs

instance Monoid Probs where
    mappend = (<>)
    mempty = Probs []

instance Semigroup PTable where
    (<>) ptable1 (PTable [] []) = ptable1
    (<>) (PTable [] []) ptable2 = ptable2
    (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
            where newEvents = combineEvents e1 e2
                  newProbs = combineProbs p1 p2

instance Monoid PTable where
    mempty = PTable [] []
    mappend = (<>)
    