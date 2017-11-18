{-# LANGUAGE FlexibleInstances #-}

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


-- Possibly

data Possibly a =
    LolNope
    | Yeppers a
    deriving (Eq, Show)

instance Functor Possibly where
    fmap _ LolNope     = LolNope
    fmap f (Yeppers a) = Yeppers (f a)

-- Short Exercise

data Sum a b =
    First a
    | Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a)  = First a
    fmap f (Second b) = Second (f b)

-- 1.

data Quant a b =
    Finance
    | Desk a
    | Bloor b

instance Functor (Quant a) where
    fmap _ Finance   = Finance
    fmap _ (Desk a)  = Desk a
    fmap f (Bloor b) = Bloor (f b)

-- 2.

data K a b =
    K a

instance Functor (K a) where
    fmap _ (K a) = K a

-- 3.
newtype Flip f a b =
    Flip (f b a)
    deriving (Eq, Show)

newtype K' a b =
    K' a
    deriving (Eq, Show)

instance Functor (Flip K' a) where
    fmap f (Flip (K' a)) = Flip (K' (f a))

-- 4.

data EvilGoateeConst a b =
    GoatyConst b
    deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst (f b)

-- 5.
data LiftItOut f a =
    LiftItOut (f a)
    deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

-- 6.

data Parappa f g a =
    DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

-- 7.

data IgnoreOne f g a b =
    IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

-- 8.

data Notoriuos g o a t =
    Notoriuos (g o) (g a) (g t)

instance Functor g => Functor (Notoriuos g o a) where
    fmap f (Notoriuos go ga gt) = Notoriuos go ga (fmap f gt)

-- 9.
data List a =
    Nil
    | Cons a (List a)
    deriving (Eq, Show)

instance Functor List where
    fmap _ Nil        = Nil
    fmap f (Cons a b) = Cons (f a) (fmap f b)

-- 10.
data GoatLord a =
    NoGoat
    | OneGoat a
    | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
    fmap _ NoGoat            = NoGoat
    fmap f (OneGoat a)       = OneGoat (f a)
    fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

-- 11.
data TalkToMe a =
    Halt
    | Print String a
    | Read (String -> a)

instance Show a => Show (TalkToMe a) where
    show Halt        = "Halt"
    show (Print s a) = "Print " ++ show s ++ " " ++ show a
    show (Read _)    = "Read"

instance Functor TalkToMe where
    fmap _ Halt        = Halt
    fmap f (Print s a) = Print s (f a)
    fmap f (Read g)    = Read (f . g)

