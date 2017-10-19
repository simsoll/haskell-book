import           Data.Time

a = foldr (++) [] ["woot", "WOOT", "woot"]
b = foldr max ' ' "fear is the little death"
c = foldr (\x -> \y -> and [x,y]) True [False, True]
d = foldr (||) True [False, True]
e = foldl (flip $ (++) . show) "" [1..5]
f = foldr (flip const) 'a' [1..5]
g = foldr (flip const) 0 "tacos"
h = foldl const 0 "burritos"
i = foldl const 'z' [1..5]

data DatabaseItem = DbString String
                    | DbNumber Integer
                    | DbDate UTCTime
                    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime
              (fromGregorian 1911 5 1)
              (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbNumber 8450
    , DbDate (UTCTime
              (fromGregorian 1921 5 1)
              (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr foldDbDate []


foldDbDate :: DatabaseItem -> [UTCTime] -> [UTCTime]
foldDbDate (DbDate utcTime) xs = utcTime : xs
foldDbDate _ xs                = xs

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr foldDbNumber []

foldDbNumber :: DatabaseItem -> [Integer] -> [Integer]
foldDbNumber (DbNumber i) xs = i : xs
foldDbNumber _ xs            = xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

average :: [Integer] -> Double
average xs = fromIntegral (sum xs) / fromIntegral (length xs)

avgDb :: [DatabaseItem] -> Double
avgDb = average . filterDbNumber

fibs = 1 : scanl (+) 1 fibs
fibs' = take 10 $ fibs
fibs'' = takeWhile (\x -> x < 100) fibs

factorial = scanl (*) 1 [2..]
factorial' n = take n $ factorial

stops = "pdtdkg"
vowels = "aeiou"

nouns = ["dog", "table", "sun"]
verbs = ["loves", "hates", "sees"]

wordGenerator :: [Char] -> [Char] -> [(Char, Char, Char)]
wordGenerator c w = [(x, y, z) | x <- c, y <- w, z <- c]

wordGenerator' :: [Char] -> [Char] -> [(Char, Char, Char)]
wordGenerator' c w = [('p', y, z) | y <- w, z <- c]

sentencesGenerator :: [String] -> [String] -> [(String, String, String)]
sentencesGenerator c w = [(x, y, z) | x <- c, y <- w, z <- c]

seekritFunc x =
    div (sum (map length (words x)))
    (length (words x))

seekritFunc' x =
    fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = myOr $ map f xs

myElem :: Eq a => a -> [a] -> Bool
myElem e xs = myOr $ map (== e) xs
myElem' e = any (== e)

myReverse :: [a] -> [a]
myReverse = foldr (\x xs -> xs ++ [x]) []

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr (\x acc -> f x : acc) [] xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = foldr (\x acc -> if f x then x : acc else acc) [] xs

squish :: [[a]] -> [a]
squish xs = foldr (\x acc -> x ++ acc) [] xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = squish $ myMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\x y -> if f x y == GT then x else y) (xs !! (length xs - 1)) xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = myMaximumBy (flip f) xs

