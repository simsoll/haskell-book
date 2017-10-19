f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))

functionC x y =
    case x > y of
        True  -> x
        False -> y

ifEvenAdd2 n =
    case even n of
        True  -> n + 2
        False -> n

nums x =
    case compare x 0 of
        LT -> -1
        GT -> 1
        EQ -> 0

dodgy :: (Num a) => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: (Num a) => a -> a
oneIsOne = dodgy 1

oneIsTwo :: (Num a) => a -> a
oneIsTwo = (flip dodgy) 2

tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div` 10
          (d, _) = divMod xLast 10

foldBold :: a -> a -> Bool -> a
foldBold a b bool =
    case bool of
        True  -> b
        False -> a

foldBold2 :: a -> a -> Bool -> a
foldBold2 a b bool
    | bool = b
    | otherwise = a

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

