module Main where

import FSQuery.Data
import FSQuery.Eval
import FSQuery.Parser
import FSQuery.UnitConvert

import System.IO
import System.Environment (getArgs)
import Data.List (intercalate, transpose)

main :: IO ()
main = do
    args <- getArgs
    case args of
      []         -> repl
      ["-h"]     -> showHelp
      ["--help"] -> showHelp
      ["-"]      -> getCommand "" >>= evalAndPrint
      _          -> evalAndPrint $ "select " ++ intercalate " " args

repl :: IO ()
repl = do
    line <- getCommand ""
    case line of
      ":q" -> return ()
      ":h" -> showHelp >> repl
      _    -> evalAndPrint line >> repl

evalAndPrint :: String -> IO ()
evalAndPrint sqlStr = do
    case parseSQL sqlStr of
      (Left parseErr) -> putStrLn $ show parseErr
      (Right sql) -> do
        { result <- evalSQL sql
        ; case result of
            (Left err) -> putStrLn err
            (Right table) -> do
              prettyPrintTable table
              putStrLn $ "(" ++ (show $ length table) ++ " lines)\n"
        }

getCommand :: String -> IO String
getCommand accu = do
    putStr "FSQuery << " >> hFlush stdout
    c <- getLine
    let new = accu ++ c
    case new of
      ""  -> getCommand ""
      " " -> getCommand ""
      ":" -> getCommand ""
      q@(":q") -> return q
      h@(":h") -> return h
      _ -> if last new == ';'
           then return new
           -- TODO Is it OK to add a space here?
           else getCommand $ new ++ " "


prettyPrintTable :: Table -> IO ()
prettyPrintTable [] = return ()
prettyPrintTable t = do
    let t' = transformTable t
    let widths = map getColumnWidth (transpose t')
    let newTable = map (rowToString widths) t'
    putStrLn $ intercalate "\n" newTable

getColumnWidth :: Row -> Int
getColumnWidth row =
    maximum $ map length [snd f | f <- row]

transformTable :: Table -> Table
transformTable [] = []
transformTable t@(h:_) = map (map f) t
    where f (s@"size", v) = (s, fileSizeToString v)
          f x = x

rowToString :: [Int] -> Row -> String
rowToString widths row =
    "| " ++ (intercalate " | " b) ++ " |"
    where a = zip widths row
          b = map f a
          f x = fieldToString (fst x) (snd x)

fieldToString :: Int -> Field -> String
fieldToString 0 (_, v) = v
fieldToString n f =
    let v = fieldToString 0 f
        p = take (n - length v) (repeat ' ')
    in v ++ p

fileSizeToString v =
    if convertFileSizeUnit v "B" >= 1024.00
      then ( bytesToHuman ((read $ takeDecimal v)::Integer)
             ++ " (" ++ v ++ ")"
           )
      else v
    where isDecimal = (`elem` ("."++['0'..'9']))
          takeDecimal = takeWhile isDecimal


showHelp :: IO ()
showHelp = putStrLn (unlines h) where
  h =
    [ ""
    , "select: Query file system using SQL."
    , ""
    , "Usage:"
    , ""
    , "  select {-h|--help}"
    , "    Show this help message."
    , ""
    , "  select"
    , "    Enter REPL."
    , ""
    , "  select -"
    , "    Read query from stdin."
    , ""
    , "  select <column names> from <dirname> [where] [order by] [limit] ;"
    , "    Use args as query. You don't need to type another 'select' in args."
    , "    <column names> can be:"
    , "      path      : full path with <dirname> stripped."
    , "      name      : name of the file, equals to basename + ['.' +] extension"
    , "      basename  : file name without extension"
    , "      extension : extension of the file, it's always empty for directory"
    , "      depth     : depth of the file, the depth of the files whose parent directory is <dirname> is 0"
    , "      size      : size of the file, a unit, like 'MiB', must be supplied when use in <where>"
    , "      atime     : last access time"
    , "      mtime     : last modification time"
    , "      ctime     : last meta data change time"
    ]
