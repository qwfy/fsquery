module System.FSQuery.Parser
    ( parseSQL
    , prop_parseSQL
    ) where

import Control.Applicative
import Control.Monad (liftM, liftM2, liftM3, when, foldM, mplus)
import Text.ParserCombinators.Parsec hiding (many, (<|>))
import Text.ParserCombinators.Parsec.Char (CharParser)
import Data.Char (toLower, toUpper)

import System.FSQuery.Data
import System.FSQuery.Util

import Test.QuickCheck
import Data.List (intercalate)


parseSQL :: String -> Either ParseError SQL
parseSQL = fmap addDepth . parse p_sql ""

addDepth :: SQL -> SQL
addDepth (Con (From xs) y) =
    Con (From zs) y
    where zs = [(fst x, d) | x <- xs]
          d = getDepth y
addDepth (Con x y) = Con (addDepth x) (addDepth y)
addDepth x = x

getDepth :: SQL -> Maybe Integer
getDepth (Where g) = getDepthFromGuard g
    where
      getDepthFromGuard :: Guard -> Maybe Integer
      getDepthFromGuard (GGroup x) =
        getDepthFromGuard x
      getDepthFromGuard (GAnd x y) =
        getDepthFromGuard x `mplus` getDepthFromGuard y
      getDepthFromGuard (GOr x y) =
        getDepthFromGuard x `mplus` getDepthFromGuard y
      getDepthFromGuard (GAtom op "depth" x) =
        let v = read x :: Integer
        in case op of
             "="  -> Just v
             "<=" -> Just v
             "<"  -> Just $ v-1
             _    -> Nothing
      getDepthFromGuard (GAtom {}) = Nothing
getDepth (Con x y) =
    getDepth x `mplus` getDepth y
getDepth x = Nothing


-- Note: Rule of Comsuming White Spaces
-- A parser whose prefix is 'p_' should always consume
-- the spaces after it parsed what it supposed to parse.

p_sql :: CharParser () SQL
p_sql = do
    -- Note: error handling
    -- If any one of these parser fails,
    -- the entire parser should fail (ParseError).
    selectC  <- p_select
    fromC    <- p_from
    whereC   <- p_where
    orderByC <- p_orderBy
    limitC   <- p_limit
    p_eQuery
    return $ foldl1 Con [fromC, whereC, orderByC, limitC, selectC]


p_select :: CharParser () SQL
p_select = do
    -- If lookAhead OK, it doesn't comsume input.
    -- If lookAhead fails, which means a p_eSelect is not present,
    -- which in turn, means that the query user supplied is not
    -- well-formed, in this case, it doesn't matter whether or not
    -- lookAhead consumes input.
    p_bSelect
    colNames <- p_commaSeparatedColumnValues
    lookAhead p_eSelect
    return $ Select colNames

p_from :: CharParser () SQL
p_from = do
    p_bFrom
    sourceNames <- p_commaSeparatedSourceValues
    lookAhead p_eFrom
    return $ From [(x, Nothing) | x<-sourceNames]

p_where :: CharParser () SQL
p_where = option Nil p_where'
p_where' = do
    lookAhead p_bWhere
    p_bWhere
    guard <- p_guard
    lookAhead p_eWhere
    return $ Where guard

p_orderBy :: CharParser () SQL
p_orderBy = option Nil p_orderBy'
p_orderBy' = do
    lookAhead p_bOrderBy
    p_bOrderBy
    x <- sepBy p_obCell (char ',' <* spaces)
    lookAhead p_eOrderBy
    return $ OrderBy x

p_limit :: CharParser () SQL
p_limit = option Nil p_limit'
p_limit' = do
    lookAhead p_bLimit
    p_bLimit
    x <- many1 digit <* spaces
    lookAhead p_eLimit
    return $ Limit (read x :: Integer)


p_guard :: CharParser () Guard
p_guard =
    chainl1 p_gUnit p_gConnector

p_gUnit :: CharParser () Guard
p_gUnit =
        p_gGroup
    <|> p_gAtom
    <?> "unit of guard"

p_gGroup :: CharParser () Guard
p_gGroup = do
    char '(' <* spaces
    c <- p_guard
    char ')' <* spaces
    return $ GGroup c

p_gConnector :: CharParser () (Guard -> Guard -> Guard)
p_gConnector = do
    c <- try (iString "and" >> skipMany1 space >> return "and")
         <|> try (iString "or" >> skipMany1 space >> return "or")
         <?> "\"AND\"/\"OR\" in WHERE clause"
    spaces
    case c of
      "and" -> return GAnd
      "or" -> return GOr

p_gAtom :: CharParser () Guard
p_gAtom = do
    k  <- p_gAtomKey
    op <- p_gAtomOp
    v  <- if k == "size" then p_gFileSize else p_gAtomValue
    return $ GAtom op k v

p_gAtomKey :: CharParser () String
p_gAtomKey = do
    key <- try p_quotedString
           <|> many1 (oneOf allowedCharInBareColumnName)
           <?> "column name in WHERE clause"
    spaces
    return key

p_gAtomOp :: CharParser () String
p_gAtomOp = do
    op <-     try (string "=")
          <|> try (string "/=")
          <|> try (string ">=")
          <|> try (string "<=")
          <|> try (string ">")
          <|> try (string "<")
          <|>     (string "~=")
          <?> "operator in WHERE clause"
    spaces
    return op

p_gAtomValue :: CharParser () String
p_gAtomValue = do
    val <- try p_quotedString
           <|> many1 (noneOf disallowedCharInBareColumnValue)
           <?> "column value in WHERE clause"
    spaces
    return val

-- TODO
-- This is used to ensure that the file size is
-- in the right format, so no error will happen
-- when converting the unit. I'm not sure this is
-- the correct way to do this (error handling).
p_gFileSize :: CharParser () String
p_gFileSize =
    try p_gQuotedFileSize
    <|> p_gBareFileSize

p_gQuotedFileSize = do
    char '"' <* spaces
    x <- many1 (oneOf $ ['0'..'9']++".") <* spaces
    y <- p_gFileSizeUnit <* spaces
    char '"' <* skipMany1 space
    return $ x++y

p_gBareFileSize = do
    x <- many1 (oneOf $ ['0'..'9']++".")
    y <- p_gFileSizeUnit
    return $ x++y

p_gFileSizeUnit :: CharParser () String
p_gFileSizeUnit = do
    x <- try (iString "b")
         <|> iString "kib"
         <|> iString "mib"
         <|> iString "gib"
         <|> iString "tib"
         <|> iString "pib"
         <|> iString "eib"
         <|> iString "zib"
         <|> iString "yib"
         <?> "unit of file size"
    spaces
    return x



-- The "b" in "p_bSelect" means "beginning of",
-- "e" in "p_eSelect" means "end of".
p_bSelect :: CharParser () String
p_bSelect = iString "select" <* skipMany1 space
p_eSelect =
    try p_bFrom
    <|> p_eQuery
    <?> "end of SELECT"

p_bFrom :: CharParser () String
p_bFrom = iString "from" <* skipMany1 space
p_eFrom =
    try p_bWhere
    <|> try p_bOrderBy
    <|> try p_bLimit
    <|> p_eQuery
    <?> "end of FROM"

p_bWhere :: CharParser () String
p_bWhere = iString "where" <* skipMany1 space
p_eWhere =
    try p_bOrderBy
    <|> try p_bLimit
    <|> p_eQuery
    <?> "end of WHERE"

p_bOrderBy :: CharParser () String
p_bOrderBy = do
    x <- iString "order" <* skipMany1 space
    y <- iString "by" <* skipMany1 space
    return $ x++y
p_eOrderBy =
    try p_bLimit
    <|> p_eQuery
    <?> "end of ORDER BY"

p_bLimit :: CharParser () String
p_bLimit = iString "limit" <* skipMany1 space
p_eLimit =
    p_eQuery
    <?> "end of LIMIT"

p_eQuery :: CharParser () String
p_eQuery =
    string ";"
    <?> "end of query"

p_commaSeparatedColumnValues :: CharParser () [String]
p_commaSeparatedColumnValues =
    sepBy p_columnCell (char ',' <* spaces)

p_commaSeparatedSourceValues :: CharParser () [String]
p_commaSeparatedSourceValues =
    sepBy p_sourceCell (char ',' <* spaces)

p_columnCell :: CharParser () FieldName
p_columnCell =
    try (many1 (oneOf allowedCharInBareColumnName) <* spaces)
    <|> p_quotedString

p_sourceCell :: CharParser () String
p_sourceCell =
    -- An unquoted value shouldn't contain space,
    -- comma (because it is used to separate values),
    -- double quote (because it's not a good idea),
    -- and semicolon (because it's used to terminate a query).
    try (many1 (noneOf ";, \"") <* spaces)
    <|> p_quotedString

p_obCell :: CharParser () (FieldName, SortOrder)
p_obCell = do
    x <- p_columnCell
    y <- p_obOrder
    return (x, y)

p_obOrder =
    (    (iString "asc"  *> return "asc")
     <|> (iString "desc" *> return "desc")
    ) <* spaces


-- Allow these characters in bare column name,
-- "bare" means no double quotes around.
allowedCharInBareColumnName = ['0'..'9']++['a'..'z']++['A'..'Z']++"_-.*"

disallowedCharInBareColumnValue = " ()\";"

-- case insensive matching
iChar c = char (toLower c) <|> char (toUpper c)
iString = mapM iChar

p_quotedString :: CharParser () String
p_quotedString = do
    char '"' <?> "open quote"
    -- Note: This would allow empty string.
    c <- many p_quotedChar
    (char '"' <* spaces) <?> "close quote"
    return c

p_quotedChar :: CharParser () Char
p_quotedChar =
    noneOf "\""
    <|> try (string "\"\"" >> return '"')



----------------------------------------------------------------------
-- QuickCheck
----------------------------------------------------------------------

prop_parseSQL :: SQLString -> Bool
prop_parseSQL sqlStr =
    case parseSQL $ show sqlStr of
      Left  _ -> False
      Right _ -> True

newtype SQLString = SQLString String

instance Show SQLString where
    show (SQLString s) = s

instance Arbitrary SQLString where
    arbitrary = do
      selectC  <- genSelect
      fromC    <- genFrom
      whereC   <- genWhere
      orderByC <- genOrderBy
      limitC   <- genLimit
      let t = [selectC, fromC, whereC, orderByC, limitC]
      let s = unwords $ filter (not . null) t
      r <- mixCase $ s ++ ";"
      return $ SQLString r

genSelect = do
    cols <- genColumns
    return $ "select " ++ intercalate "," cols

genFrom = do
    srcs <- genSources
    return $ "from " ++ intercalate "," srcs

genColumns :: Gen [String]
genColumns = resize 20 x
    where x = listOf1 genOneColumn

genOneColumn :: Gen String
genOneColumn = do
    let cols = [ "path", "name", "basename", "extension"
               , "depth", "size", "atime", "mtime", "ctime" ]
    let quotedCols = map doubleQuote cols
    elements $ cols ++ quotedCols

genSources :: Gen [String]
genSources = resize 1 x
    where x = listOf1 genSource

genSource = resize 20 x
    where x = listOf1 $ elements (['a'..'z'] ++ ['A'..'Z'])

genWhere = do
    g <- genGuards
    return $ if null g
             then ""
             else "where " ++ g

genAtomGuard = do
    col <- genOneColumn
    op <- elements ["=", ">", "<", "/=" , ">=" , "<=" , "~="]
    let genVal = case trimWhile (=='"') col of
                   "size"  -> genFileSize
                   "depth" -> genNatural
                   "atime" -> genTime
                   "ctime" -> genTime
                   "mtime" -> genTime
                   _       -> genString
    val <- genVal
    return $ foldl1 (++) [col, op, val]

genAtomGuards = do
    x <- listOf1 genAtomGuard
    let y = filter (not . null) x
    chainGuard genConnector y

genGroupGuard = do
    x <- genAtomGuards
    return $ "(" ++ x ++ ")"

genUnitGuard = oneof [genAtomGuard, genGroupGuard]

genGuards = do
    x <- listOf genUnitGuard
    let y = filter (not . null) x
    chainGuard genConnector y

genConnector = elements [" and ", " or "]

chainGuard :: Gen String -> [String] -> Gen String
chainGuard _ [] = return ""
chainGuard _ [x] = return x
chainGuard c (x:y:rest) = do
    h <- chainGuard2 c [x, y]
    chainGuard c (h:rest)

chainGuard2 :: Gen String -> [String] -> Gen String
chainGuard2 g [x,y] = do
    c <- g
    return $ x ++ c ++ y

genOrderBy = do
    x <- genOrderBys
    return $ if null x
             then ""
             else "order by " ++ intercalate "," x

genOrderBys = resize 10 x
    where x = listOf genOneOrderBy
genOneOrderBy = do
    col <- genOneColumn
    ord <- elements ["asc", "desc"]
    return $ col ++ " " ++ ord

genLimit = do
    g <- elements [True, False]
    if not g
    then return ""
    else do
      n <- elements [0..65536]
      return $ "limit " ++ show n


genNatural = oneof [genPositiveInteger, fmap (:[]) genDigit]
genPositiveInteger = do
    h <- genDigit1
    t <- listOf genDigit
    return $ h:t

genFloat = oneof [genNatural, x] where
    x = do
      h <- genNatural
      t <- listOf1 $ elements digits
      return $ h ++ "." ++ t

genDigit = elements digits
genDigit1 = elements digits1

genFileSizeUnit =
    elements [ "B"  , "KiB", "MiB"
             , "GiB", "TiB", "PiB"
             , "EiB", "ZiB", "YiB" ]

genFileSize = do
    n <- genFloat
    u <- genFileSizeUnit
    return $ n++u

genTime = do
    y  <- elements $ map show [1900, 3000]
    m  <- genInt2 1 12
    d  <- genInt2 1 31
    hh <- genInt2 0 23
    mm <- genInt2 0 59
    ss <- genInt2 0 59
    let date = intercalate "-" [y, m, d]
    let time = intercalate ":" [hh, mm, ss]
    return $ doubleQuote (date ++ " " ++ time)

genInt2 :: Int -> Int -> Gen String
genInt2 start stop = do
    x <- elements $ map show [start, stop]
    return $ if length x == 1 then '0':x else x


genString = oneof [genBareString, genQuotedString]

genBareString = listOf1 $ elements s
    where s = concat [digits, letters, ".$^"]

genQuotedString = fmap doubleQuote genBareString


mixCase :: String -> Gen String
mixCase = mapM f
    where f c = do
            u <- elements [True, False]
            return $ if u then toUpper c else toLower c
