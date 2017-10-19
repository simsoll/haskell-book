isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs x = if x < 0 then -1 * x else x

f :: (a,b) -> (c,d) -> ((b,d), (a,c))
f x y = ((snd x, snd y), (fst x, fst y))

x = (+)

lengthPlusOne xs = w `x` 1
    where w = length xs

myId x = x

myFst (a,b) = a
