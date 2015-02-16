module FSSQL.Parser (parseSQL) where

import Control.Applicative
import Control.Monad (liftM, liftM2, liftM3)
import Text.ParserCombinators.Parsec hiding (many, (<|>))
import Text.ParserCombinators.Parsec.Char (CharParser)

import FSSQL.Data


parseSQL :: String -> Either ParseError SQL
parseSQL input = parse p_sql "" input


p_sql :: CharParser () SQL
p_sql = do
    spaces
    selectC <- p_select
    fromC <- p_from
    whereC <- p_where
    p_eQuery
    return $ fromC `Con` whereC `Con` selectC


p_select :: CharParser () SQL
p_select = do
    p_bSelect
    liftM Select p_commaSeparatedColumnValues

p_from :: CharParser () SQL
p_from = do
    p_bFrom
    liftM From p_commaSeparatedSourceValues

p_where :: CharParser () SQL
p_where = do
    isWherePresent <- try p_bWhere
                      <|> return "no"
    case isWherePresent of
      "no" -> return Dummy
      _ -> liftM Where p_guard

p_guard :: CharParser () Guard
p_guard = do
    chainl1 p_gUnit p_gConnector

p_gUnit :: CharParser () Guard
p_gUnit =
    try p_gGroup
    <|> p_gAtom

p_gGroup :: CharParser () Guard
p_gGroup = do
    char '(' <* spaces
    c <- p_guard
    char ')' <* spaces
    return $ GGroup c

p_gConnector :: CharParser () (Guard -> Guard -> Guard)
p_gConnector = do
    c <- try (string "and")
         <|> string "or"
         <?> "\"and\"/\"or\" in where clause"
    spaces
    case c of
      "and" -> return GAnd
      "or" -> return GOr

-- parse one guard, like
--   key = "quoted value"
--   key /= unquoted_value
p_gAtom :: CharParser () Guard
p_gAtom = do
    k <- p_gAtomKey
    op <- p_gAtomOp
    v <- p_gAtomValue
    return $ GAtom op k v

p_gAtomKey :: CharParser () String
p_gAtomKey = do
    key <- try p_quotedString
           -- <|> many1 (noneOf "() =\"")
           <|> many1 (oneOf allowedBareColumnName)
           <?> "column name in where clause"
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
           <|> many1 (noneOf "; ()\"")
           <?> "column value in where clause"
    spaces
    return val

p_quotedString :: CharParser () String
p_quotedString = do
    char '"'
    c <- many p_quotedChar -- this would allow empty string
    (char '"' <* spaces) <?> "close quote"
    return c

p_quotedChar :: CharParser () Char
p_quotedChar =
    noneOf "\""
    <|> try (string "\"\"" >> return '"')

-- The "b" in "p_bSelect" means "beginning of",
-- "e" in "p_eSelect" means "end of".
p_bSelect :: CharParser () String
p_bSelect = string "select" <* spaces
p_eSelect = p_bFrom

p_bFrom :: CharParser () String
p_bFrom = string "from" <* spaces
p_eFrom = p_bWhere
      <|> p_bOrderBy
      <|> p_bLimit
      <|> p_eQuery

p_bWhere :: CharParser () String
p_bWhere = string "where" <* spaces
p_eWhere = p_bOrderBy
       <|> p_bLimit
       <|> p_eQuery

p_bOrderBy :: CharParser () String
p_bOrderBy = do
    { x <- string "order " <* spaces
    ; y <- string "by"
    ; return $ x++y
    } <* spaces
p_eOrderBy = p_bLimit
         <|> p_eQuery

p_bLimit :: CharParser () String
p_bLimit = string "limit" <* spaces
p_eLimit = p_eQuery

p_eQuery :: CharParser () String
p_eQuery = string ";" <* spaces

p_commaSeparatedColumnValues :: CharParser () [String]
p_commaSeparatedColumnValues = sepBy p_columnCell (char ',' <* spaces)

p_commaSeparatedSourceValues :: CharParser () [String]
p_commaSeparatedSourceValues = sepBy p_sourceCell (char ',' <* spaces)

p_columnCell :: CharParser () String
p_columnCell =
    try (many1 (oneOf allowedBareColumnName) <* spaces)
    <|> p_quotedString

p_sourceCell :: CharParser () String
p_sourceCell =
    -- An unquoted value shouldn't contain space,
    -- comma (because it is used to separate values),
    -- double quote (because it's not a good idea),
    -- and semicolon (because it's used to terminate a query).
    try (many1 (noneOf ";, \"") <* spaces)
    <|> p_quotedString

-- Allow these characters in bare column name,
-- "bare" means no double quotes around.
allowedBareColumnName = ['0'..'9']++['a'..'z']++['A'..'Z']++"_-.*"
