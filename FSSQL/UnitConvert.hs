module FSSQL.UnitConvert
    ( convertStorageUnit
    , epochToLocaleHuman
    , bytesToHuman
    ) where

import qualified Data.Char as DChar
import Control.Applicative (liftA, liftA2)

import System.Locale
import Data.Time
import Data.Time.Format

import Numeric (showFFloat)

epochToLocaleHuman :: Integer -> IO String
epochToLocaleHuman seconds = do
    tz <- getCurrentTimeZone
    let epoT = show seconds
    let utcT = readTime defaultTimeLocale "%s" epoT :: UTCTime

    return $ show $ utcToLocalTime tz utcT

convertStorageUnit :: String -> String -> Double
convertStorageUnit src destUnit' =
    let isDigit = (`elem` ['0'..'9'] ++ ".")
        srcSize = read (takeWhile isDigit src) :: Double
        srcUnit = toLower $ dropWhile (==' ') (dropWhile isDigit src)
        destUnit = toLower destUnit'
        scale = case liftA (1024^^)  (liftA2 (-) (lookup srcUnit alist) (lookup destUnit alist)) of
                  Nothing -> error "Can not convert unit."
                  Just x -> x
        alist = [ ("b",    0)
                , ("kib",  1)
                , ("mib",  2)
                , ("gib",  3)
                , ("tib",  4)
                , ("pib",  5)
                , ("eib",  6)
                , ("zib",  7)
                , ("yib",  8) ]
    in srcSize * scale

bytesToHuman :: Integer -> String
bytesToHuman b =
    let e = floor ((logBase 1024 $ fromIntegral b) :: Double)
        newSize = (fromIntegral b / (1024 ^ e)) :: Double
        alist = [ (0, "B")
                , (1, "KiB")
                , (2, "MiB")
                , (3, "GiB")
                , (4, "TiB")
                , (5, "PiB")
                , (6, "EiB")
                , (7, "ZiB")
                , (8, "YiB") ]
    in case lookup e alist of
         Nothing -> show b ++ "B"
         Just x -> showFFloat (Just 2) newSize "" ++ x
        


toLower :: String -> String
toLower "" = ""
toLower (x:xs) = DChar.toLower x : toLower xs
