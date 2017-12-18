import           Data.Monoid

-- Library Functions

-- 1.

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

-- 2.

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

-- 3.

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a = getAny . foldMap (\x -> Any $ x == a)

-- 4.

maybeOrd :: Ord a => (a -> a -> a) -> a -> Maybe a -> Maybe a
maybeOrd _ a Nothing  = Just a
maybeOrd f a (Just b) = Just $ f a b

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr (maybeOrd min) Nothing

-- 5.

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr (maybeOrd max) Nothing

-- 6.

null' :: (Foldable t) => t a -> Bool
null'= foldr (\_ _ -> True) False

-- 7.

length' :: (Foldable t) => t a -> Int
length' = foldr (const $ (+1)) 0

-- 8.

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

-- 9.

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id


-- 10.

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\a b -> mappend b $ f a) mempty

-- chapter exercises

-- 1.

data Constant a b =
    Constant b

instance Foldable (Constant a) where
    foldMap f (Constant b) = f b

-- 2.

data Two a b =
    Two a b

instance Foldable (Two a) where
    foldMap f (Two a b) = f b

-- 3.

data Three a b c =
    Three a b c

instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c

-- 4.

data Three' a b =
    Three' a b b

instance Foldable (Three' a) where
    foldMap f (Three' a b1 b2) = mappend (f b1) (f b2)

-- 5.

data Four' a b =
    Four' a b b b

instance Foldable (Four' a) where
    foldMap f (Four' a b1 b2 b3) = mappend (mappend (f b1) (f b2)) (f b3)

-- filterF

filterF :: ( Applicative f
           , Foldable t
           , Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF predicate = foldMap (\x -> if predicate x then pure x else mempty)
