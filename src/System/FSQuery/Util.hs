module System.FSQuery.Util where

import Data.Char (isSpace)
import qualified Data.Char as DChar

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace


toLower :: String -> String
toLower "" = ""
toLower (x:xs) = DChar.toLower x : toLower xs
