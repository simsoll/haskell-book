import           Data.Char
import           PoemLines (split)

myEnumFromTo :: (Enum a, Eq a) => a -> a -> [a]
myEnumFromTo from to
    | from == to = [to]
    | otherwise = from : (myEnumFromTo (succ from) to)

eftBool :: Bool -> Bool -> [Bool]
eftBool = myEnumFromTo

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = myEnumFromTo

eftInt :: Int -> Int -> [Int]
eftInt = myEnumFromTo

eftChar :: Char -> Char -> [Char]
eftChar = myEnumFromTo

myWords :: [Char] -> [[Char]]
myWords = split ' '

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

combineAsTuples fst snd = [(x,y) | x <- fst, y <- snd]

combineAsTuplesConditioned fst snd predicate count
    = take count [(x,y) | x <- fst, y <- snd, predicate x, predicate y ]

itIsMystery xs =
    map (\x -> elem x "aeiou") xs

myZip :: [a] -> [b] -> [(a,b)]
myZip = zipWith (,)

onlyUpper :: [Char] -> [Char]
onlyUpper = filter isUpper

upperFirst :: [Char] -> [Char]
upperFirst (x:xs) = toUpper x : xs

allCaps :: [Char] -> [Char]
allCaps []     = []
allCaps (x:xs) = toUpper x : allCaps xs

firstCapped :: [Char] -> Char
firstCapped = toUpper . head

myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = myOr $ map f xs

myElem :: Eq a => a -> [a] -> Bool
myElem x = any (== x)

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = reverse xs ++ [x]

squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []     = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy compare [x, y] = if compare x y == GT then x else y
myMaximumBy compare (x:y:xs) =
    case compare x y of
        GT -> myMaximumBy compare (x:xs)
        _  -> myMaximumBy compare (y:xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy compare = myMaximumBy (flip compare)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
