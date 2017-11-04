module Exercises where

import           Test.QuickCheck


-- 1.

half :: Fractional a => a -> a
half x = x /2

halfIdentity :: Double -> Double
halfIdentity = (*2) . half

-- main :: IO ()
-- main = quickCheck (\x -> halfIdentity x == 1)


-- 3.
plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: Int -> Int -> Bool
plusCommutative x y = x + y == y + x

-- 4.
multiAssociative :: Int -> Int -> Int -> Bool
multiAssociative x y z = x * (y * z) == (x * y) * z

multiCommutative :: Int -> Int -> Bool
multiCommutative x y = x * y == y * x


main :: IO()
main = quickCheck multiCommutative
