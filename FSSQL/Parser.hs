module FSSQL.Parser (parseSQL) where

import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, (<|>))

import FSSQL.Data


parseSQL :: String -> Either ParseError SQL
parseSQL input = parse p_oneSQL "(unknown)" input


p_oneSQL :: GenParser Char st SQL
p_oneSQL = do
    try p_selectFromWhere
    <|> p_selectFrom

p_selectFrom = do
    selectC <- p_select <* spaces
    fromC <- p_from <* spaces
    char ';' <* spaces
    return (Con fromC selectC)

p_selectFromWhere = do
    selectC <- p_select <* spaces
    fromC <- p_from <* spaces
    whereC <- p_where
    char ';' <* spaces
    return (Con (Con fromC whereC) selectC)

p_select = do
    string "select" <* spaces
    cols <- p_cells
    return (Select cols)

p_from = do
    string "from" <* spaces
    srcs <- p_cells
    return (From srcs)

p_where = do
    string "where" <* spaces
    guards <- p_guards
    return (Where guards)

p_guards = do
    left <- try p_gGroup
            <|> p_gAtom

    c <- try p_guardEOGuard
         <|> p_guardConnector
         <?> "<another guard connector or end of guard>"
    case c of
      ";" -> return left
      ")" -> return left
      _ -> do
             spaces <?> "spaces after 'and'/'or'"
             right <- p_guards
             return $ case c of
                        "and" -> (GAnd left right)
                        "or" -> (GOr left right)

p_guardEOGuard = do
    (lookAhead (string ";") <|> string ")") <* spaces

p_guardConnector = ((string "and") <|> (string "or")) <* spaces

p_gGroup = do
    char '(' <* spaces
    expr <- p_guards
    -- char ')' <* spaces
    return (GGroup expr)

-- parse one guard: key = "value here"
p_gAtom = do
    key <- many (noneOf "=() ") <* spaces
    op <- (    string "="
           <|> string "/="
           <|> string ">="
           <|> string "<="
           <|> string ">"
           <|> string "<"
          ) <* spaces
    char '"' <?> "<open quote for value in guard>"
    val <- many (noneOf "\"")
    (char '"' <* spaces) <?> "<close quote for value in guard>"
    return (GAtom op key val)

p_cells = sepBy p_cell (char ',' <* spaces)
p_cell = (many (noneOf ";, ")) <* spaces
