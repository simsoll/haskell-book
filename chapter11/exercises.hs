{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Maybe


data Price =
    Price Integer deriving (Eq, Show)

data Size =
    Size Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata
    deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited
    deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline Size
    deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 30)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _           = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

newtype Goats =
    Goats Int deriving (Eq, Show, TooMany)

newtype IntString =
    IntString (Int, String) deriving (Eq, Show)

instance TooMany IntString where
    tooMany (IntString (n, _)) = n > 42

newtype IntTwice =
    IntTwice (Int, Int) deriving (Eq, Show)

instance TooMany IntTwice where
    tooMany (IntTwice (a, b)) = a + b > 42

data OperatingSystem =
    GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgLang =
    Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show)

data Programmer =
    Programmer { os   :: OperatingSystem
               , lang :: ProgLang }
    deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill
    , Mac
    , Windows
    ]

allLanguages :: [ProgLang]
allLanguages =
    [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer os lang | os <- allOperatingSystems, lang <- allLanguages]

data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right)
    = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
    Node (Node Leaf 3 Leaf)
         1
         (Node Leaf 4 Leaf)

mapExpected =
    Node (Node Leaf 4 Leaf)
         2
         (Node Leaf 5 Leaf)

mapOkay =
    if mapTree (+1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"

testTree :: BinaryTree Integer
testTree =
    Node (Node Leaf 1 Leaf)
         2
         (Node Leaf 3 Leaf)

preorder :: BinaryTree a -> [a]
preorder Leaf                = []
preorder (Node left a right) = a : (preorder left ++ preorder right)

testPreorder :: IO ()
testPreorder =
    if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears"

inorder :: BinaryTree a -> [a]
inorder Leaf                = []
inorder (Node left a right) = preorder left ++ [a] ++ preorder right

testInorder :: IO ()
testInorder =
    if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears"

postorder :: BinaryTree a -> [a]
postorder Leaf                = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

testPostorder :: IO ()
testPostorder =
    if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "Bad news bears"

main :: IO()
main = do
    testPreorder
    testInorder
    testPostorder

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f x tree = foldr f x (preorder tree)


capitalizeWord :: String -> String
capitalizeWord []     = []
capitalizeWord (x:xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph word = intercalate ". " $ map capitalizeWord $ splitOn ". " word

type Digit = Char
type Presses = Int

data DaPhone = DaPhone [(Digit, [Char])]

aPhone :: DaPhone
aPhone = DaPhone [ ('1',[])
                 , ('2', "abc2")
                 , ('3', "def3")
                 , ('4', "ghi4")
                 , ('5', "jkl5")
                 , ('6', "mno6")
                 , ('7', "pqrs7")
                 , ('8', "tuv8")
                 , ('9', "wxyz")
                 , ('*', "^")
                 , ('0', " ")
                 , ('#', ".,")
                 ]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps = reverseTapsHelper []

reverseTapsHelper :: [(Digit, Presses)] -> DaPhone -> Char -> [(Digit, Presses)]
reverseTapsHelper acc phone c =
    if isUpper c
        then reverseTapsHelper (('*', 1) : acc) phone (toLower c)
        else let (digit, keys) = toPhoneKey phone c
                 index = elemIndex c keys
                 presses = if isJust index then 1 + fromJust index else 0
             in acc ++ [(digit, presses)]

toPhoneKey :: DaPhone -> Char -> (Digit, [Char])
toPhoneKey _ ' ' = ('0', " ")
toPhoneKey (DaPhone (x:xs)) c =
        if (any (== toLower c) (snd x))
        then x
        else toPhoneKey (DaPhone xs) c

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone sentence = concat $ map (reverseTaps phone) sentence

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

mostPopularLetter :: String -> Char
mostPopularLetter = fst . (mostPopularCount ' ')

mostPopularLetterCost :: String -> Int
mostPopularLetterCost s = let (c, n) = (mostPopularCount ' ') s
                              pressesPerCount = sum . map snd $ reverseTaps aPhone c
                          in n * pressesPerCount

mostPopularCount :: (Foldable t, Eq a) => a -> t a -> (a, Int)
mostPopularCount init = (maxCount init) . foldr countFolder []

maxCount :: a -> [(a, Int)] -> (a, Int)
maxCount init = foldr (\(x,y) (z,w) -> if (y > w) then (x,y) else (z,w)) (init, 0)

countFolder :: Eq a => a -> [(a, Int)] -> [(a, Int)]
countFolder c [] = [(c, 1)]
countFolder c ((x,y):xs) =
    if x == c
        then (x,y+1):xs
        else (x,y):(countFolder c xs)

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord = fst . (mostPopularCount " ")
