module Main where

import System.FSQuery.Data
import System.FSQuery.Eval
import System.FSQuery.Parser
import System.FSQuery.UnitConvert
import System.FSQuery.Util (trim)

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
      _          -> evalAndPrint $ args !! 0


repl :: IO ()
repl = do
    cmd <- getCommand ""
    case cmd of
      ":q" -> return ()
      ":h" -> showHelp >> repl
      _    -> evalAndPrint cmd >> repl


evalAndPrint :: String -> IO ()
evalAndPrint sqlStr = case parseSQL sqlStr of
    (Left parseErr) ->
      putStrLn $ show parseErr
    (Right sql) -> do
      result <- evalSQL sql
      case result of
        (Left err) ->
          putStrLn err
        (Right table) -> do
          prettyPrintTable table
          putStrLn $ "(" ++ (show $ length table) ++ " lines)\n"


getCommand :: String -> IO String
getCommand accu = do
    putStr "FSQuery << " >> hFlush stdout
    c <- getLine
    let line = trim $ accu ++ c
    case line of
      ""  -> getCommand ""
      ":" -> getCommand ""
      ";" -> getCommand ""
      q@(":q") -> return q
      h@(":h") -> return h
      _ -> if last line == ';'
           -- Add a space before the terminatting semicolon
           then return $ init line ++ " ;"
           -- Replace new line with a space
           else getCommand $ line ++ " "


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
    , "FSQuery: Query file system using SQL."
    , ""
    , ""
    , "Usage"
    , "-----"
    , ""
    , "To show help message, including the format of SQL, use '-h', or '--help':"
    , ""
    , "    fsquery {-h|--help}"
    , ""
    , "To enter REPL mode, specify no arguments:"
    , ""
    , "    fsquery"
    , ""
    , "To read <query> from stdin, use '-' as argument:"
    , ""
    , "    fsquery -"
    , ""
    , "A <query> can also be supplied in the argument, like this:"
    , ""
    , "    fsquery 'select path, size from ./foo;'"
    , ""
    , ""
    , "Definition of <query>"
    , "---------------------"
    , ""
    , "A <query> is a semicolon-terminated string, here is an example of it:"
    , ""
    , "    SELECT path, size"
    , "    FROM ./foo/bar"
    , "    WHERE (size > 1.7mib AND name ~= dog.*) OR"
    , "          (size < 1.7mib AND name = cat.txt)"
    , "    ORDER BY atime DESC"
    , "    LIMIT 7;"
    , ""
    , "The keywords, such as SELECT and FROM, are case insensitive."
    , ""
    , "<query>"
    , ":= SELECT <column name> [, ...]"
    , "   FROM <directory>"
    , "   [WHERE <filter> [{AND|OR} ...]]"
    , "   [ORDER BY <order specification> [, ...]]"
    , "   [LIMIT <count>]"
    , "   ;"
    , ""
    , "<column name>"
    , ":= [path|name|basename|extension|depth|size|atime|mtime|ctime]"
    , ""
    , "    path      : Path of the file, with <directory> stripped, it will never begin with '/'."
    , "    name      : Name of the file, including extension (if it has one)."
    , "    basename  : Name without extension."
    , "    extension : Extension of the file, without the dot. it's always an empty string for directory."
    , "    depth     : Depth of the file, the depth of the files whose parent directory is <directory> is 0."
    , "    size      : Size of the file."
    , "    atime     : Last access time."
    , "    mtime     : Last modification time."
    , "    ctime     : Last meta data change time."
    , ""
    , "<directory>"
    , "    A path on file system, Like '/var/log/' or 'C:/Program Files'."
    , ""
    , "<filter>"
    , ":= <column name> <operator> <value>"
    , ""
    , "    A parentheses, '()', can also be put around a <filter>, or multiple <filter>s connected with {and|or}."
    , ""
    , "Some examples of valid <filter>:"
    , ""
    , "    path /= foo/bar/baz.txt"
    , "    path = \"people/buzz lightyear/\""
    , "    name ~= ^hello.*$"
    , "    extension = txt"
    , "    depth > 1"
    , "    size >= 1.5mib"
    , "    size < \"7 GiB\""
    , "    atime > \"2013-07-27 23:59:59\""
    , ""
    , "<operator>"
    , ":= [=|>|<|/=|>=|<=|~=]"
    , ""
    , "    =  : test for equality"
    , "    >  : greater than"
    , "    <  : less than"
    , "    /= : not equal to"
    , "    >= : greater than or equal to"
    , "    <= : less than or equal to"
    , "    ~= : match using POSIX extended regular expression"
    , ""
    , "<value>"
    , ":= {<string>|<quoted string>}"
    , ""
    , "    It can be a <string>, which is just a list of characters, if it's simple enough."
    , ""
    , "    Or it can be a <quoted string>, which is a <string> surrounded by double quotes, if it contains characters that could confuse the program, such as space, semicolon, keyword, etc.."
    , ""
    , "    To use a double quote ('\"') in a value, use '\"\"', so, '\"foo\"\"bar\"' actually means 'foo\"bar'."
    , ""
    , "    If <column name> is 'size', then a <value> must begin with an number, follows by a unit, which can be one of 'B', 'KiB', 'MiB', 'GiB', 'TiB', 'PiB', 'EiB', 'ZiB' or 'YiB', (case insensitive). You can also put the entire value in a double quote, in which case, spaces are also allowed."
    , ""
    , "    If <column name> is 'atime', 'mtime' or 'ctime', then the value must in the format of '\"YYYY-mm-dd HH:MM:SS\"'."
    , ""
    , "<order specification>"
    , ":= <column name> {ASC|DESC}"
    , ""
    , "<count>"
    , "    It is a natural number."
    ]
