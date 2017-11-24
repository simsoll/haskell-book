import           Control.Monad (join)

-- The answer is the exercise

bind :: Monad m => (a -> m b) -> m a -> m b
bind f a = join $ fmap f a

-- Either Monad

data Sum a b =
    First a
    | Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap f (First a)  = First a
    fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
    pure = Second
    First a <*> _ = First a
    _ <*> First a = First a
    Second f <*> Second b = Second $ f b

instance Monad (Sum a) where
    return = pure
    First a >>= _ = First a
    Second b >>= f = f b

-- Chapter Exercises (monads)

-- 1.
data Nope a =
    NopeDotJpg
    deriving(Show)

instance Functor Nope where
    fmap f _ = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    _ <*> _ = NopeDotJpg

instance Monad Nope where
    return = pure
    NopeDotJpg >>= _ = NopeDotJpg

-- 2.
data PhhhbbtttEither b a =
    Left' a
    | Right' b

instance Functor (PhhhbbtttEither b) where
    fmap _ (Right' b) = Right' b
    fmap f (Left' a)  = Left' $ f a

instance Applicative (PhhhbbtttEither b) where
    pure = Left'
    Right' b <*> _ = Right' b
    _ <*> Right' b = Right' b
    Left' f <*> Left' a = Left' $ f a

instance Monad (PhhhbbtttEither b) where
    return = pure
    Right' b >>= _ = Right' b
    Left' a >>= f = f a

-- 3.

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity a = Identity $ f a

instance Monad Identity where
    return = pure
    Identity a >>= f = f a

-- 4.
data List a =
    Nil
    | Cons a (List a)
    deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x $ xs `append` ys


instance Functor List where
    fmap _ Nil        = Nil
    fmap f (Cons a b) = Cons (f a) (fmap f b)

instance Applicative List where
    pure a = Cons a (Nil)
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    Cons f Nil <*> Cons a as = Cons (f a) (Cons f Nil <*> as)
    Cons f fs <*> Cons a as = Cons (f a) (Cons f Nil <*> as) `append` (fs <*> Cons a as)

instance Monad List where
    return = pure
    Nil >>= _ = Nil
    a >>= f = join $ fmap f a
