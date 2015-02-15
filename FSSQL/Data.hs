module FSSQL.Data where


data SQL = Select [String]
         | From [String]
         | Where Guard
         | Con SQL SQL
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
