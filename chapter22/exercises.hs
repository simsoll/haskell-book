{-# LANGUAGE InstanceSigs #-}

import           Data.Char

-- Warming up

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
    capped <- cap
    reversed <- rev
    return (capped, reversed)

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = cap >>= (\x -> rev >>= (\y -> return $ (,) x y))

-- Ask

newtype Reader r a =
    Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap f (Reader ra) = Reader $ (f . ra)

ask :: Reader a a
ask = Reader id

-- Reading Comprehension

-- 1.

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

-- 2.

asks :: (r -> a) -> Reader r a
asks = Reader

-- 3.

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader $ \r -> a

    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

