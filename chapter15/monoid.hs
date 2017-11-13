module MyMonoid where

import           Control.Monad
import           Data.Monoid
import           Test.QuickCheck

import           Data.Monoid     hiding ((<>))
import           Data.Semigroup  hiding (mappend)
import           MySemigroup

-- Optional Monoid

data Optional a =
    Nada
    | Only a
    deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend (Only x) (Only y) = Only $ mappend x y
    mappend (Only x) _        = Only x
    mappend _ (Only y)        = Only y
    mappend _ _               = Nada

-- Madness

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
    e <> "! he said " <>
    adv <> " as he jumped into his car " <>
    noun <> " and drove off with his " <>
    adj <> " wife."

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj =
    mconcat [e, "! he said ",
        adv, " as he jumped into his car ",
        noun, " and drove off with his ",
        adj, " wife."]

-- Testing QuickCheck's patience

data Bull =
    Fools
    | Twoo
    deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary = frequency [ (1, return Fools), (1, return Twoo)]

instance Monoid Bull where
    mempty = Fools
    mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- main :: IO ()
-- main = do
--     let ma = monoidAssoc :: BullMappend
--         mli = monoidLeftIdentity
--         mlr = monoidRightIdentity
--     quickCheck (ma :: BullMappend)
--     quickCheck (mli :: Bull -> Bool)
--     quickCheck (mlr :: Bull -> Bool)

-- Maybe Another Monoid

newtype First' a =
    First' {getFirst' :: Optional a}
    deriving (Eq, Show)

instance Monoid (First' a) where
    mempty = First' Nada
    mappend (First' (Only x)) _             = First' $ Only x
    mappend (First' Nada) (First' (Only y)) = First' $ Only y
    mappend _ _                             = First' Nada

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = do
        a <- arbitrary
        frequency [(1, return $ First' Nada), (3, return $ First' (Only a))]

-- 1.




type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

main :: IO ()
main = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)
