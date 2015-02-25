module System.FSQuery.UnitConvert
    ( unifyFileSize
    , convertFileSizeUnit
    , epochToLocaleHuman
    , bytesToHuman
    ) where

import Control.Applicative (liftA, liftA2)
import System.Locale
import Data.Time
import Data.Time.Format
import Numeric (showFFloat)

import System.FSQuery.Util (toLower)

epochToLocaleHuman :: Integer -> IO String
epochToLocaleHuman seconds = do
    tz <- getCurrentTimeZone
    let epoT = show seconds
    let utcT = readTime defaultTimeLocale "%s" epoT :: UTCTime

    return $ show $ utcToLocalTime tz utcT

unifyFileSize :: String -> String -> (Double, Double)
unifyFileSize machine human =
    let (hsize, hdigits, hunit) = breakFileSize human
        msize = convertFileSizeUnit machine hunit
    in mapTuple (\ x -> read x :: Double)
           (showFFloat (Just hdigits) msize "", hsize)

breakFileSize :: String -> (String, Int, String)
breakFileSize s' = (size, precision, unit)
    where
      s = filter (/= ' ') s'
      digits = ['0'..'9']
      alphas = ['a'..'z']++['A'..'Z']
      p = (reverse . dropWhile (`elem` alphas) . reverse . dropWhile (`elem` digits)) s
      isDecimal = (`elem` (digits++"."))
      size = takeWhile isDecimal s
      unit = toLower $ dropWhile isDecimal s
      precision = case p of
                    "" -> 0
                    ('.':x) -> length x

convertFileSizeUnit :: String -> String -> Double
convertFileSizeUnit src destUnit' =
    let (size, _, srcUnit) = breakFileSize src
        srcSize = read size :: Double
        destUnit = toLower destUnit'
        scale = case liftA (1024^^)  (liftA2 (-) (lookup srcUnit alist) (lookup destUnit alist)) of
                  Nothing -> error "Can not convert unit."
                  Just x -> x
        alist = [ ("b",   0)
                , ("kib", 1)
                , ("mib", 2)
                , ("gib", 3)
                , ("tib", 4)
                , ("pib", 5)
                , ("eib", 6)
                , ("zib", 7)
                , ("yib", 8) ]
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


mapTuple :: (a->b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)
