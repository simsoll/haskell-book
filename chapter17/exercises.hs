import           Data.List   (elemIndex)
import           Data.Monoid

-- Lookups

-- 1.

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])

-- 2.

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3.
x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y'

-- 4.

xs = [1, 2, 3]
ys = [4, 5, 6]

x' :: Maybe Integer
x' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x <*> y

-- Identity Instance

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity a = Identity (f a)

-- Constant Instance

newtype Constant a b =
    Constant { getConstant :: a}
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap f (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
    pure a = Constant mempty
    (Constant a) <*> (Constant a') = Constant (mappend a a')

-- Fixer Upper

-- 1.

f = const <$> Just "Hello" <*> pure "World"

-- 2.
g = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

-- List Applicative
data List a =
    Nil
    | Cons a (List a)
    deriving (Eq, Show)

instance Functor List where
    fmap _ Nil        = Nil
    fmap f (Cons a b) = Cons (f a) (fmap f b)


instance Applicative List where
    pure a = Cons a (Nil)
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    Cons f Nil <*> Cons a as = Cons (f a) (Cons f Nil <*> as)
    Cons f fs <*> Cons a as = Cons (f a) (Cons f Nil <*> as) `append` (fs <*> Cons a as)

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

-- ZipList Applicative

newtype ZipList' a =
    ZipList' (List a)
    deriving (Eq, Show)

instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
    pure a = ZipList' (Cons a Nil)
    ZipList' f <*> ZipList' b = ZipList' (zipList' f b)

zipList' :: List (a -> b) -> List a -> List b
zipList' Nil _                     = Nil
zipList' _ Nil                     = Nil
zipList' (Cons f Nil) (Cons a Nil) = Cons (f a) Nil
zipList' (Cons f fs) (Cons a as)   = Cons (f a) (zipList' fs as)

