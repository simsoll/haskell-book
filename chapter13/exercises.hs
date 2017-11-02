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

-- 4.

type Name = String
type Age = Integer
data Person = Person Name Age deriving Show

data PersonInvalid =
        NameEmpty
        | AgeTooLow
        | PersonInvalidUnknown String
        deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
 | name /= "" && age > 0 = Right $ Person name age
 | name == "" = Left NameEmpty
 | not (age > 0) = Left AgeTooLow
 | otherwise =
        Left $ PersonInvalidUnknown $
         "Name was: " ++ show name ++
         " Age was: " ++ show age

showPerson :: Either PersonInvalid Person -> String
showPerson (Left error)   = "Error: " ++ show error
showPerson (Right person) = "Yay! " ++ show person

gimmePerson :: IO ()
gimmePerson = do
        putStr "Name: "
        name <- getLine
        putStr "Age: "
        age <- getLine
        putStrLn $ showPerson $ mkPerson name (read age :: Integer)
