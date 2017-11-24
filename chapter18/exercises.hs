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
    pure b = Second b
    First a <*> _ = First a
    _ <*> First a = First a
    Second f <*> Second b = Second $ f b

instance Monad (Sum a) where
    return = pure
    First a >>= _ = First a
    Second b >>= f = f b
