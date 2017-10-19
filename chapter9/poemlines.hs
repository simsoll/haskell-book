module PoemLines (split) where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

split :: Char -> [Char] -> [[Char]]
split _ ""    = []
split splitter words =
    let
        cleanWords = dropWhile (== splitter) words
    in
        (takeWhile (/= splitter) cleanWords) : (split splitter (dropWhile (/= splitter) cleanWords))

myLines :: String -> [String]
myLines = split '\n'

shouldEqual =
    [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry"
    ]

main :: IO ()
main =
    print $
    "Are they equal? "
    ++ show (myLines sentences == shouldEqual)
