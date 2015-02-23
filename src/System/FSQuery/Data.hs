module System.FSQuery.Data where


data SQL = Select [String]
         | From [String]
         | Where Guard
         | OrderBy [(FieldName, SortOrder)]
         | Limit Integer
         | Con SQL SQL
         | Nil
         deriving (Show)


data Guard = GAtom CompareOperator FieldName FieldValue
           | GAnd Guard Guard
           | GOr Guard Guard
           | GGroup Guard
           deriving (Show)


type Table = [Row]
type Row = [Field]
type Field = (String, String)


type FieldName = String
type FieldValue = String
type CompareOperator = String


type SortOrder = String
type OrderSpec = (FieldName, SortOrder)
