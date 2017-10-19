{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

example = 1

-- f = (*9) 6
-- f = head [(0,"doge"),(1,"kitteh")]
-- f = head [(0 :: Integer,"doge"),(1,"kitteh")]
-- f = if False then True else False
-- f = length [1,2,3,4,5]
-- f = (length [1,2,3,4]) > (length "TACOCAT")

-- x = 5
-- y = x + 5
-- w = y * 10

-- x = 5
-- y = x + 5
-- z y = y * 10

-- x = 5
-- y = x + 5
-- f = 4 / y

-- x = "Julie"
-- y = " <3 "
-- z = "Haskell"
-- f = x ++ y ++ z

bigNum = (^) 5 $ 10
-- wahoo = bigNum $ 10

-- x = print
-- y = print "woohoo!"
-- z = x "hello world!"

functionH :: [a] -> a
functionH (x:_) = x

functionC :: (Ord a) => a -> a -> Bool
functionC x y =
    if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h x = g (f x)


data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e x = w (q x)

data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f g a = fst (g (f a))
