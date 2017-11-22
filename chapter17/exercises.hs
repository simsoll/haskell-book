import           Control.Applicative (liftA3)
import           Data.List           (elemIndex)
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

-- Variations on Either
data Validation e a =
    Failure e
    | Success a
    deriving (Eq, Show)

instance Functor (Validation e) where
    fmap f (Failure e) = Failure e
    fmap f (Success a) = Success $ f a

instance Monoid e => Applicative (Validation e) where
    pure a = Success a
    Failure e1 <*> Failure e2 = Failure (mappend e1 e2)
    Failure e <*> _ = Failure e
    _ <*> Failure e = Failure e
    Success f <*> Success a = Success $ f a

-- Chapter Exercises

-- 1.
data Pair a = Pair a a deriving Show

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure a = Pair a a
    Pair f g <*> Pair a b = Pair (f a) (g b)

-- 2.
data Two a b = Two a b deriving (Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
    pure b = Two mempty b
    Two f g <*> Two a b = Two (mappend f a) (g b)

-- 3.
data Three a b c = Three a b c deriving (Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure c = Three mempty mempty c
    Three f g h <*> Three a b c = Three (mappend f a) (mappend g b) (h c)

-- 4.
data Three' a b = Three' a b b deriving (Show)

instance Functor (Three' a) where
    fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance Monoid a => Applicative (Three' a) where
    pure b = Three' mempty b b
    Three' f g1 g2 <*> Three' a b1 b2 = Three' (mappend f a) (g1 b1) (g2 b2)

-- 5.
data Four a b c d = Four a b c d deriving (Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure d = Four mempty mempty mempty d
    Four f g h i <*> Four a b c d = Four (mappend f a) (mappend g b) (mappend h c) (i d)

-- 6.
data Four' a b = Four' a a a b deriving (Show)

instance Functor (Four' a) where
    fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance (Monoid a) => Applicative (Four' a) where
    pure b = Four' mempty mempty mempty b
    Four' f1 f2 f3 g <*> Four' a1 a2 a3 b = Four' (mappend f1 a1) (mappend f2 a2) (mappend f3 a3) (g b)

-- Combinations
combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
