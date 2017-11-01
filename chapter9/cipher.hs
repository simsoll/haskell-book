module Cipher where

import           Data.Char

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

cipher :: Int -> [Char] -> [Char]
cipher n = map (rightShift n)

unCipher :: Int -> [Char] -> [Char]
unCipher n = map (leftShift n)

main :: IO ()
main = do
    putStr "Insert Caesar key: "
    key <- getLine
    putStr "Insert word to be ciphered: "
    word <- getLine
    putStrLn $ "Ciphered word: " ++ (cipher (read key :: Int) word)
