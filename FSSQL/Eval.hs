module FSSQL.Eval (evalSQL) where

import Control.Monad (forM, liftM2)

import FSSQL.Data
import FSSQL.FileMeta
import FSSQL.UnitConvert


evalSQL :: SQL -> IO Table
evalSQL = eval []


eval :: Table -> SQL -> IO Table

eval t (Con l r) = eval t l >>= flip eval r

eval _ (Select []) = error "No columns specified."
eval rs (Select ["*"]) = return rs
eval rs (Select cols) = return $ map interestedCols rs
    where interestedCols row =
            [kv | kv <- row, fst kv `elem` cols]

eval [] (From sources) = getTableFromMany sources

eval rs (Where (GAtom op k v)) = return rest where
    -- "machine" stands the value got from file system,
    -- it's usually a result of a function call.
    --
    -- "human" means the value user specified in a SQL query.
    --
    -- For example, in path = readme.txt,
    -- "path" conveys the value of "machine",
    -- and "readme.txt" is the value of "human".
    --
    -- Convert "machine" and "human" to string, (if needed),
    -- apply the function identified with "op" to them.
    --
    -- The convertion is needed, (sometimes), because some
    -- values, like size of a file, cannot be compared with
    -- (readOp op), to get around with this, we compare them
    -- somewhere else, say function f, then return a fake one,
    -- so that when compared with (readOp op), it produces the
    -- the same result with f.
    --
    -- f :: String -> String -> Bool
    f machine human = (readOp op) m h where
      (m, h) =
        case k of
          "size" -> anyToStringTuple op
                      (convertStorageUnit machine "B")
                      (convertStorageUnit human "B")
          _ -> (machine, human)

    g maybeMachine =
        case maybeMachine of
          Just x -> f x
          Nothing ->
            error $ "Using " ++ k
                    ++ " as LHS in a WHERE clause is not supported."

    t r = g (lookup k r) v

    rest = filter t rs

eval rs (Where (GGroup expr)) =
    eval rs (Where expr)

eval rs (Where (GAnd g1 g2)) =
    eval rs (Where g1) >>= flip eval (Where g2)

eval rs (Where (GOr g1 g2)) =
    liftM2 (++)
      (eval rs (Where g1))
      (eval rs (Where g2))


readOp "="  = (==)
readOp "/=" = (/=)
readOp ">=" = (>=)
readOp "<=" = (<=)
readOp ">"  = (>)
readOp "<"  = (<)


anyToStringTuple :: Ord a => CompareOperator -> a -> a -> (String, String)
anyToStringTuple op l r
    | (readOp op) l r =
        case op of
          "="  -> aa
          ">"  -> ba
          "<"  -> ab
          "/=" -> ab
          ">=" -> ba
          "<=" -> ab
    | otherwise =
        case op of
          "="  -> ab
          ">"  -> ab
          "<"  -> ba
          "/=" -> aa
          ">=" -> ab
          "<=" -> ba
    where
      aa = ("a", "a")
      ab = ("a", "b")
      ba = ("b", "a")
