cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haa"

sumUpTo :: (Eq a, Num a) => a -> a
sumUpTo 0 = 0
sumUpTo n = n + sumUpTo (n-1)

multiply :: (Integral a) => a -> a -> a
multiply x 0 = 0
multiply x y = x + multiply x (y-1)

data DividedResult a =
    Result (a, a)
  | DividedByZero
  deriving (Show)

-- dividedBy :: Integral a => a -> a -> DividedResult a
-- dividedBy num denom = go num denom 0
--     where go n d count
--            | d == 0 = DividedByZero
--            | 0 < n && n < d = Result (count, n)
--            | otherwise = go (n - d) d (count + 1)

mc91 :: Integral a => a -> a
mc91 n
 | n > 100 = n - 10
 | otherwise = mc91 $ mc91 $ n + 11
