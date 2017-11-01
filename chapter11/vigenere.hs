module Vigenere where

import           Data.Char
import           Data.List.Split

rightShift :: Int -> Char -> Char
rightShift _ ' ' = ' '
rightShift 0 c = c
rightShift n c =
    let cInt = ord c
        aInt = ord 'a'
        zInt = ord 'z'
        (cDiv, cMod) = divMod (cInt + n) (zInt + 1)
    in chr $ aInt * cDiv + cMod

leftShift :: Int -> Char -> Char
leftShift n c =
    rightShift (ord 'z' - ord 'a' - n + 1) c

vigenere :: [Char] -> [Char] -> [Char]
vigenere source key = zipWith rightShift (shifts source key) source

unVigenere :: [Char] -> [Char] -> [Char]
unVigenere source key = zipWith leftShift (shifts source key) source

shifts :: [Char] -> [Char] -> [Int]
shifts source key =
        map (\x -> ord x - ord 'a') $ map (indexMapper key) (indexed source key)

indexMapper :: [Char] -> Maybe Int -> Char
indexMapper _ Nothing        = 'a'
indexMapper key (Just index) = key !! index

indexed :: [Char] -> [Char] -> [Maybe Int]
indexed source key =
    let keyLength = length key
    in reverse $ fst $ helper key source ([], 0)

helper :: [Char] -> [Char] -> ([Maybe Int], Int) -> ([Maybe Int], Int)
helper _ [] y = y
helper key (x:xs) (acc, i) =
    case x of
        ' ' -> helper key xs (Nothing : acc, i)
        _   -> helper key xs ((Just $ mod i (length key)) : acc, i + 1)

main :: IO ()
main = do
    putStr "Insert Vigeneré key: "
    key <- getLine
    putStr "Insert word to be ciphered: "
    word <- getLine
    putStrLn $ "Ciphered word: " ++ (vigenere word key)
