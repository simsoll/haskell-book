import           Data.List

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s     = Just s

replaceThe :: String -> String
replaceThe s = intercalate " " $ replaceThe' $ map notThe $ words s

replaceThe' :: [Maybe String] -> [String]
replaceThe' [] = []
replaceThe' (x:xs) = case x of
    Nothing -> "a" : (replaceThe' xs)
    Just s  -> s : (replaceThe' xs)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = countTheBeforeVowel' $ map notThe $ words s

countTheBeforeVowel' :: [Maybe String] -> Integer
countTheBeforeVowel' [] = 0
countTheBeforeVowel' [x] = 0
countTheBeforeVowel' (Nothing:(Just y):xs) = if startsWithVowel y then 1 + countTheBeforeVowel' xs else countTheBeforeVowel' xs
countTheBeforeVowel' (_:_:xs) = countTheBeforeVowel' xs

startsWithVowel :: String -> Bool
startsWithVowel [] = False
startsWithVowel s  = isVowel $ s !! 0

isVowel :: Char -> Bool
isVowel x = x `elem` "aeiou"

countVowels :: String -> Int
countVowels = length . filter isVowel

newtype Word' =
    Word' String
    deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord s = if 2 * countVowels s > (length $ filter ((==) ' ') s) then Nothing else Just $ Word' s

data Nat =
    Zero
    | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger = natToInteger' 0

natToInteger' :: Integer -> Nat -> Integer
natToInteger' x Zero       = x
natToInteger' x (Succ nat) = natToInteger' (x+1) nat

integerToNat :: Integer -> Maybe Nat
integerToNat i
    | i < 0 = Nothing
    | otherwise = Just $ integerToNat' Zero i

integerToNat' :: Nat -> Integer -> Nat
integerToNat' nat 0 = nat
integerToNat' nat i = Succ (integerToNat' nat (i-1))

-- Small library for Maybe
-- 1.

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- 2.
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing  = x
mayybee x f (Just y) = f y

-- 3.
fromMaybe :: a -> Maybe a -> a
fromMaybe x = mayybee x id

-- 4.
listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

-- 5.
catMaybes :: [Maybe a] -> [a]
catMaybes = concat . map maybeToList

-- 6.
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = if any isNothing xs
                then Nothing
                else Just $ catMaybes xs


-- Small library for Either
-- 1.
lefts' :: [Either a b] -> [a]
lefts' = foldr lefts'' []

lefts'' :: Either a b -> [a] -> [a]
lefts'' (Right _) xs = xs
lefts'' (Left x) xs  = x:xs


-- 2.
rights' :: [Either a b] -> [b]
rights' = foldr rights'' []

rights'' :: Either a b -> [b] -> [b]
rights'' (Left _) xs  = xs
rights'' (Right x) xs = x:xs

-- 3.
paritionEithers' :: [Either a b] -> ([a], [b])
paritionEithers' xs = (lefts' xs, rights' xs)

-- 4.
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right x) = Just $ f x

-- 5.
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x)  = f x
either' _ g (Right y) = g y

-- 6.
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' _ (Left _)  = Nothing
eitherMaybe'' f (Right x) = Just $ f x

-- unfolds
-- 1.
myIterate :: (a -> a) -> a -> [a]
myIterate f x = f x : (myIterate f (f x))

-- 2.
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
    Nothing     -> []
    Just (y, z) -> y : (myUnfoldr f z)

-- 3.
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\y -> Just (f y, f y)) x

-- binary tree
data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

-- 1.
unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x = case f x of
    Nothing        -> Leaf
    Just (y, z, w) -> Node (unfold f y) z (unfold f w)

-- 2.
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (treeBuild' n) n

treeBuild' :: Integer -> Integer -> Maybe (Integer, Integer, Integer)
treeBuild' n 0 = Nothing
treeBuild' n i = Just (i-1, n-i, i-1)
