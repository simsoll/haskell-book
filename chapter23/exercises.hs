{-# LANGUAGE InstanceSigs #-}

import           Control.Applicative       (liftA3)
import           Control.Monad             (replicateM)
import           Control.Monad.Trans.State
import           System.Random

-- Roll Your Own

data Die =
      DieOne
    | DieTwo
    | DieThree
    | DieFour
    | DieFive
    | DieSix
    deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
    case n of
        1 -> DieOne
        2 -> DieTwo
        3 -> DieThree
        4 -> DieFour
        5 -> DieFive
        6 -> DieSix
        -- Use 'error'
        -- _extremely_ sparingly.
        x ->
            error $
                "intToDie got non 1-6 integer: "
                ++ show x

-- 1.

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
    where
     go :: Int -> Int -> StdGen -> Int
     go sum count gen
        | sum >= n = count
        | otherwise =
          let (die, nextGen) =
                randomR (1, 6) gen
          in go (sum + die)
                (count + 1) nextGen

-- 2.

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 (0, []) g
    where
     go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
     go sum (count, dies) gen
        | sum >= n = (count, dies)
        | otherwise =
          let (die, nextGen) =
                randomR (1, 6) gen
          in go (sum + die)
                (count + 1, (intToDie die) : dies) nextGen

-- Write State for yourself

newtype Moi s a =
    Moi { runMoi :: s -> (a, s)}

instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi g) = Moi $ (\(a, s) -> (f a, s)) . g

instance Applicative (Moi s) where
    pure :: a -> Moi s a
    pure a = Moi $ (\s -> (a, s))

    (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
    (Moi f) <*> (Moi g) = Moi $ (\(ab, s) -> (ab (fst $ g s), s)) . f

instance Monad (Moi s) where
    return = pure

    (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
    (Moi f) >>= g = Moi $ (\(a, s) -> runMoi (g a) s) . f

-- Chapter exercises

-- 1.

get' :: Moi s s
get' = Moi $ (\s -> (s, s))

-- 2.

put' :: s -> Moi s ()
put' s = Moi $ (\_ -> ((), s))

-- 3.

exec' :: Moi s a -> s -> s
exec' (Moi sa) s = snd $ sa s

-- 4.

eval' :: Moi s a -> s -> a
eval' (Moi sa) s = fst $ sa s

-- 5.

modify'' :: (s -> s) -> Moi s ()
modify'' f = Moi $ (\s -> ((), f s))
