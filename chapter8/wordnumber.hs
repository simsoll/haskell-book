module WordNumber where

import           Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n =
    case n of
        1 -> "one"
        2 -> "two"
        3 -> "three"
        4 -> "four"
        5 -> "five"
        6 -> "six"
        7 -> "seven"
        8 -> "eight"
        9 -> "nine"
        _ -> "zero"

digits :: Int -> [Int]
digits n = divModded n []
    where divModded number arr =
            case divMod number 10 of
                (0, element)         -> [element] ++ arr
                (newNumber, element) -> divModded newNumber ([element] ++ arr)


wordNumber :: Int -> String
wordNumber n =
    concat $ intersperse "-" $ map digitToWord $ digits n
