
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
