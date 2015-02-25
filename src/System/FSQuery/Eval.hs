module System.FSQuery.Eval (evalSQL) where

import Control.Monad (forM, liftM, liftM2)
import Data.Maybe (fromJust)
import Data.List (sortBy, genericTake, isInfixOf)
import Text.Regex.TDFA

import System.FSQuery.Data
import System.FSQuery.FileMeta
import System.FSQuery.UnitConvert


evalSQL :: SQL -> IO (Either String Table)
evalSQL = eval $ Right []


eval :: (Either String Table) -> SQL -> IO (Either String Table)

eval left@(Left x) _ = return left

eval t Nil = return t

eval t (Con l r) = eval t l >>= flip eval r

eval _ (Select []) = return $ Right []
eval rs (Select ["*"]) = return rs
eval (Right rs) (Select cols) = return (Right selected)
    where
      selected =  map (reOrder . interestedCols) rs
      interestedCols row = [kv | kv <- row, fst kv `elem` cols]
      reOrder row = [(k, fromJust $ lookup k row) | k <- cols]

eval (Right []) (From sources) =
    fmap Right (getTableFromMany sources)


eval t (Where (GGroup expr)) =
    eval t (Where expr)

eval t (Where (GAnd g1 g2)) =
    eval t (Where g1) >>= flip eval (Where g2)

eval t (Where (GOr g1 g2)) = do
    x <- eval t (Where g1)
    y <- eval t (Where g2)
    case (x, y) of
      (Right xt, Right yt) -> return (Right $ xt++yt)
      (e@(Left _), _) -> return e
      (_, e@(Left _)) -> return e

-- Choose those rows from [table] which make guard [k op v] true.
-- If there is an error when evaluating the guard, a [Left] is returned.
eval empty@(Right []) (Where (GAtom _ _ _)) = return empty
eval (Right table@(h:_)) (Where (GAtom op k v))
    | lookup k h == Nothing =
        let errMsg = "There is no column named '" ++ k ++ "' in result set."
        in return $ Left errMsg
    | otherwise = return (Right result)
        where
        { result = filter (isGuardTrue op k v) table
        ; isGuardTrue operator key queryV row
            | key == "size" = (readOpForSize operator) machineV queryV
            | otherwise = (readOp operator) machineV queryV
            where machineV = fromJust $ lookup key row
        }

eval t (OrderBy []) = return t
eval right@(Right []) (OrderBy _) = return right
eval (Right t@(r:_)) (OrderBy orderSpecs) = do
    let colInQuery = [snd f | f <- orderSpecs]
        colInTable = [snd f | f <- r]
    case colInQuery `isSubsetOf` colInTable  of
      True -> return (Right $ sortBy (compareRow orderSpecs) t)
      False -> return (Left $ "Some columns in the ORDER BY clause "
                              ++ "is not in the result set, so you "
                              ++ "can't sort based on them.")

eval t (Limit n) =
    return $ liftM (genericTake n) t

readOp :: String -> (String -> String -> Bool)
readOp "="  = (==)
readOp ">"  = (>)
readOp "<"  = (<)
readOp "/=" = (/=)
readOp ">=" = (>=)
readOp "<=" = (<=)
readOp "~=" = \str pat -> str =~ pat :: Bool

readOpForSize :: String -> (String -> String -> Bool)
readOpForSize "=" = \m h ->
    let (m', h') = unifyFileSize m h
    in m' == h'
readOpForSize ">" = \m h ->
    let (m', h') = unifyFileSize m h
    in m' > h'
readOpForSize "<" = \m h ->
    let (m', h') = unifyFileSize m h
    in m' < h'
readOpForSize ">=" = \m h ->
    let (m', h') = unifyFileSize m h
    in m' >= h'
readOpForSize "<=" = \m h ->
    let (m', h') = unifyFileSize m h
    in m' <= h'

isSubsetOf :: (Eq a) => [a] -> [a] -> Bool
xs `isSubsetOf` ys = null $ filter (`elem` ys) xs

-- Compare two rows using dictionary order, with dictionary being
-- a list of column names.
compareRow :: [OrderSpec] -> Row -> Row -> Ordering
compareRow [] _ _ = EQ
compareRow ((colName, colOrder):rest) r1 r2 =
    let x1 = fromJust $ lookup colName r1
        x2 = fromJust $ lookup colName r2
    in case compare' colName colOrder x1 x2 of
         { EQ -> compareRow rest r1 r2
         ; z -> z
         }

compare' :: FieldName -> SortOrder -> String -> String -> Ordering
compare' "size" sortOrder x y =
    flipOrdering sortOrder $ compare x' y'
    where x' = (read $ init x) :: Integer
          y' = (read $ init y) :: Integer
compare' _ sortOrder x y =
    flipOrdering sortOrder $ compare x y

flipOrdering :: SortOrder -> Ordering -> Ordering
flipOrdering "asc" x = x
flipOrdering "desc" x =
    case x of
      EQ -> EQ
      LT -> GT
      GT -> LT
