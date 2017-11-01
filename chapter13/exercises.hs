-- 3.

import           Control.Monad
import           Data.Char     (toLower)
import           System.Exit   (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    case (isPalindrome line1) of
        True  -> putStrLn "It's a plaindrome!"
        False -> exitSuccess

isPalindrome :: String -> Bool
isPalindrome s = reverse cleanedWord == cleanedWord
        where cleanedWord = cleanWord s

cleanWord :: String -> String
cleanWord s = filter (\c -> c /= ' ' && c /= '\'') $ map toLower s

