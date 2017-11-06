import           Data.Semigroup
import           Test.QuickCheck

-- 1.

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
    (a <> (b <> c)) == ((a <> b) <> c)

-- 2.

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    Identity a <> Identity b = Identity $ a <> b

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

-- 3.

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

-- 4.

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three a b c) <> (Three d e f) = Three (a <> d) (b <> e) (c <> f)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c


-- 5.

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
    (Four a b c d) <> (Four e f g h) = Four (a <> e) (b <> f) (c <> g) (d <> h)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four a b c d


-- 6.

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    BoolConj True <> BoolConj True = BoolConj True
    _ <> _  = BoolConj False

instance Arbitrary BoolConj where
    arbitrary = frequency [ (1, return $ BoolConj True), (1, return $ BoolConj False)]

-- 7.

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    BoolDisj False <> BoolDisj False = BoolDisj False
    BoolDisj _ <> BoolDisj _ = BoolDisj True

instance Arbitrary BoolDisj where
    arbitrary = frequency [(1, return $ BoolDisj True), (1, return $ BoolDisj False)]

-- 8.

data Or a b = Fst a | Snd b  deriving (Eq, Show)

instance Semigroup (Or a b) where
    (Snd a) <> _ = Snd a
    _ <> (Snd b) = Snd b
    _ <> (Fst a) = Fst a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        frequency [(1, return $ Fst a), (1, return $ Snd b)]

type Assoc x = x -> x -> x -> Bool

main :: IO()
main = do
    quickCheck (semigroupAssoc :: (Assoc Trivial))
    quickCheck (semigroupAssoc :: (Assoc (Identity String)))
    quickCheck (semigroupAssoc :: (Assoc (Two String String)))
    quickCheck (semigroupAssoc :: (Assoc (Three String String String)))
    quickCheck (semigroupAssoc :: (Assoc (Four String String String String)))
    quickCheck (semigroupAssoc :: (Assoc BoolConj))
    quickCheck (semigroupAssoc :: (Assoc BoolDisj))
    quickCheck (semigroupAssoc :: (Assoc (Or String String)))

