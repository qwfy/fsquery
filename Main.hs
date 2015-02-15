module Main where

import FSSQL.Data
import FSSQL.Eval
import FSSQL.Parser
import FSSQL.UnitConvert

import System.IO
import Data.List (intercalate)

main :: IO ()
main = repl

repl :: IO ()
repl = do
    putStr "FSSQL << "
    hFlush stdout
    line <- getLine
    let sql = case parseSQL line of
                Left err -> error $ show err
                Right x -> x
    table <- evalSQL sql
    putStrLn "-------"
    putTable table
    putStrLn "-------"
    putStrLn $ (show $ length table) ++ " lines\n"
    repl


putTable :: Table -> IO ()
putTable [] = return ()
putTable rows =
    putStrLn $ intercalate "\n" (map rowToString rows)

rowToString :: Row -> String
rowToString row = intercalate " | " (map fieldToString row)

fieldToString :: Field -> String
fieldToString (k, v) = -- k ++ "=" ++
    case k of
      "size" ->
        if convertStorageUnit v "B" >= 1024.00
          then ( bytesToHuman ((read $ takeDecimal v)::Integer)
                 ++ " (" ++ v ++ ")"
               )
          else v
      _ -> v

    where isDecimal = (`elem` ("."++['0'..'9']))
          takeDecimal = takeWhile isDecimal
