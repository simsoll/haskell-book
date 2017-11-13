module MySemigroup where

import           Data.Semigroup
import           Test.QuickCheck

-- 1.

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial


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

-- 9.

newtype Combine a b = Combine { unCombine :: (a -> b)}

instance (Semigroup b) => Semigroup (Combine a b) where
    (Combine f) <> (Combine g) = Combine (\a -> f a <> g a)

instance Show (Combine a b) where
    show (Combine _) = "Combine f"

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = do
      f <- arbitrary
      return $ Combine f

-- 10.

newtype Comp a = Comp { unComp :: (a -> a)}

instance Semigroup (Comp a) where
    (Comp f) <> (Comp g) = Comp (f . g)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
    arbitrary = do
        f <- arbitrary
        return $ Comp f

instance Show (Comp a ) where
    show (Comp _) = "Comp f"

-- 11.

data Validation a b = MyFailure a | MySuccess b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    MySuccess x <> _ = MySuccess x
    _ <> MySuccess y = MySuccess y
    MyFailure x <> MyFailure y = MyFailure (x <> y)


instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        frequency [(1, return $ MyFailure a), (1, return $ MySuccess b)]



type Assoc x = x -> x -> x -> Bool
type FuncAssoc f x = f -> f -> f -> x -> Bool

semigroupAssoc :: (Eq m, Semigroup m) => Assoc m
semigroupAssoc a b c =
    (a <> (b <> c)) == ((a <> b) <> c)

semigroupFuncAssocCombine :: (Eq b, Semigroup b) => FuncAssoc (Combine a b) a
semigroupFuncAssocCombine f g h a = unCombine ((f <> g) <> h) a == unCombine (f <> (g <> h)) a

semigroupFuncAssocComp :: (Eq a) => FuncAssoc (Comp a) a
semigroupFuncAssocComp f g h a = unComp ((f <> g) <> h) a == unComp (f <> (g <> h)) a


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
    quickCheck (semigroupFuncAssocCombine :: (FuncAssoc (Combine String String) String))
    quickCheck (semigroupFuncAssocComp :: (FuncAssoc (Comp String) String))
    quickCheck (semigroupAssoc :: (Assoc (Validation String String)))

