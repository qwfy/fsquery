module System.FSQuery.Util where

import Data.Char (isSpace)
import qualified Data.Char as DChar

trimWhile :: (Char -> Bool) -> String -> String
trimWhile p = f . f
    where f = reverse . dropWhile p

toLowerString :: String -> String
toLowerString "" = ""
toLowerString (x:xs) = DChar.toLower x : toLowerString xs

digits = ['0'..'9']
digits1 = tail digits
letters = ['a'..'z'] ++ ['A'..'Z']

doubleQuote :: String -> String
doubleQuote x = q ++ x ++ q where q = "\""
