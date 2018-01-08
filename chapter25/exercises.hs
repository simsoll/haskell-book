-- instance types provided as
-- they may help.

{-# LANGUAGE InstanceSigs #-}

import           Control.Applicative

newtype Compose f g a =
    Compose { getCompose :: f (g a) }
    deriving (Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure a = Compose $ pure (pure a)

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (Compose f) <*> (Compose a) = Compose $ liftA2 (<*>) f a

-- Compose Instances

-- 1.

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap f (Compose fga) = (foldMap . foldMap) f fga

-- 2.

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga

-- Bifunctor

class Bifunctor p where
    {-# MINIMAL bimap | first, second #-}
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g

    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id

    second :: (b -> c) -> p a b -> p a c
    second = bimap id

-- 1.

data Deux a b = Deux a b
    deriving (Show)

instance Bifunctor Deux where
    bimap f g (Deux a b) = Deux (f a) (g b)

-- 2.

data Const' a b = Const' a
    deriving (Show)

instance Bifunctor Const' where
    bimap f _ (Const' a) = Const' (f a)

-- 3.

data Drei a b c = Drei a b c
    deriving (Show)

instance Bifunctor (Drei a) where
    bimap f g (Drei a b c) = Drei a (f b) (g c)

-- 4.

data SuperDrei a b c = SuperDrei a b
    deriving (Show)

instance Bifunctor (SuperDrei a) where
    bimap f _ (SuperDrei a b) = SuperDrei a (f b)

-- 5.

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
    bimap _ _ (SemiDrei a) = SemiDrei a

-- 6.

data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
    bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

-- 7.

instance Bifunctor Either where
    bimap f _ (Left a)  = Left (f a)
    bimap _ g (Right b) = Right (g b)
