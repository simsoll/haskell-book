
-- Heavy Lifting

-- 1.

a :: [Int]
a = fmap (+1) $ read "[1]"

-- 2.

b :: Maybe [String]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3.

c :: Int -> Int
c = (*2) . (\x -> x - 2)

-- 4.

d :: Int -> String
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

-- 5.

-- e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123"++)) (fmap show ioi)
    in fmap (*3) changed


-- Instances of Func

-- 1.
newtype Identity a = Identity a deriving (Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

-- 2.
data Pair a = Pair a a deriving (Show)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

-- 3.
data Two a b = Two a b deriving (Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

-- 4.
data Three a b c = Three a b c deriving (Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

-- 5.
data Three' a b = Three' a b b deriving (Show)

instance Functor (Three' a) where
    fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

-- 6.
data Four a b c d = Four a b c d deriving (Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

-- 7.
data Four' a b = Four' a a a b deriving (Show)

instance Functor (Four' a) where
    fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)
