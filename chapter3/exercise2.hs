module Exercise2 where

yell :: String -> String
yell a = a ++ "!"

fourthLetter :: String -> Char
fourthLetter a = a !! 4

lastNine :: String -> String
lastNine = drop 9

thirdLetter :: String -> Char
thirdLetter a = a !! 3

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

rvrs :: String
rvrs = let x = "Curry is awesome"
           awesome = drop 9 x
           is = take 2 $ drop 6 x
           curry = take 5 x
       in  awesome ++ " " ++ is ++ " " ++ curry
