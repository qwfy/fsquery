module FSQuery.Parser (parseSQL) where

import Control.Applicative
import Control.Monad (liftM, liftM2, liftM3, when)
import Text.ParserCombinators.Parsec hiding (many, (<|>))
import Text.ParserCombinators.Parsec.Char (CharParser)
import Data.Char (toLower, toUpper)

import FSQuery.Data


parseSQL :: String -> Either ParseError SQL
parseSQL input = parse p_sql "" input

-- Note: rule of comsuming white spaces
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
    return $ From sourceNames

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
    x <- many digit
    lookAhead p_eLimit
    return $ Limit (read x :: Integer)

--------------------------------------

p_guard :: CharParser () Guard
p_guard = do
    chainl1 p_gUnit p_gConnector

p_gUnit :: CharParser () Guard
p_gUnit = do
        p_gGroup
    <|> p_gAtom
    <?> "unit of guard"

p_gGroup :: CharParser () Guard
p_gGroup = do
    char '(' <* spaces
    c <- p_guard
    char ')' <* skipMany1 space
    return $ GGroup c

p_gConnector :: CharParser () (Guard -> Guard -> Guard)
p_gConnector = do
    c <- try (string "and" <* skipMany1 space)
         <|> try (string "or" <* skipMany1 space)
         <?> "\"and\"/\"or\" in where clause"
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
          <|> (string "<")
          <?> "operator in where clause"
    spaces
    return op

p_gAtomValue :: CharParser () String
p_gAtomValue = do
    val <- try p_quotedString
           <|> many1 (noneOf disallowedCharInBareColumnValue)
           <?> "column value in where clause"
    spaces
    return val

-- TODO
-- This is used to ensure that the file size is
-- in the right format, so no error will happen
-- when converting the unit. I'm not sure this is
-- the correct way to do this (error handling).
p_gFileSize :: CharParser () String
p_gFileSize = do
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
p_bSelect = string "select" <* skipMany1 space
p_eSelect =
    try p_bFrom
    <|> p_eQuery
    <?> "end of SELECT"

p_bFrom :: CharParser () String
p_bFrom = string "from" <* skipMany1 space
p_eFrom =
    try p_bWhere
    <|> try p_bOrderBy
    <|> try p_bLimit
    <|> p_eQuery
    <?> "end of FROM"

p_bWhere :: CharParser () String
p_bWhere = string "where" <* skipMany1 space
p_eWhere =
    try p_bOrderBy
    <|> try p_bLimit
    <|> p_eQuery
    <?> "end of WHERE"

p_bOrderBy :: CharParser () String
p_bOrderBy = do
    x <- string "order" <* skipMany1 space
    y <- string "by" <* skipMany1 space
    return $ x++y
p_eOrderBy =
    try p_bLimit
    <|> p_eQuery
    <?> "end of ORDER BY"

p_bLimit :: CharParser () String
p_bLimit = string "limit" <* skipMany1 space
p_eLimit =
    p_eQuery
    <?> "end of LIMIT"

p_eQuery :: CharParser () String
p_eQuery =
    string ";"
    <?> "end of query"

p_commaSeparatedColumnValues :: CharParser () [String]
p_commaSeparatedColumnValues = sepBy p_columnCell (char ',' <* spaces)

p_commaSeparatedSourceValues :: CharParser () [String]
p_commaSeparatedSourceValues = sepBy p_sourceCell (char ',' <* spaces)

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
    (iString "asc" <|> iString "desc") <* spaces


-- Allow these characters in bare column name,
-- "bare" means no double quotes around.
allowedCharInBareColumnName = ['0'..'9']++['a'..'z']++['A'..'Z']++"_-.*"

disallowedCharInBareColumnValue = " ()\";"

-- case insensive matching
iChar c = char (toLower c) <|> char (toUpper c)
iString s = mapM iChar s

p_quotedString :: CharParser () String
p_quotedString = do
    char '"' <?> "open quote"
    -- Note: This would allow empty string.
    c <- many p_quotedChar
    (char '"' <* skipMany1 space) <?> "close quote"
    return c

p_quotedChar :: CharParser () Char
p_quotedChar =
    noneOf "\""
    <|> try (string "\"\"" >> return '"')
