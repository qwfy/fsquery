module FSSQL.FileMeta
    ( getTableFromOne
    , getTableFromMany
    ) where


import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), takeBaseName, takeExtension)
import Data.List (nub, (\\))
import Control.Monad (forM, liftM)

import Data.String.Utils (replace)
import Data.List (stripPrefix, isSuffixOf)

import System.PosixCompat.Files
    ( getFileStatus
    , fileSize
    , accessTime
    , modificationTime
    , statusChangeTime
    , isRegularFile
    , isDirectory
    , isSymbolicLink
    , FileStatus
    )

import FSSQL.Data
import FSSQL.UnitConvert

getTableFromMany :: [FilePath] -> IO Table
getTableFromMany [] = return []
getTableFromMany (x:xs) = do
    t <- getTableFromOne x
    liftM (t++) (getTableFromMany xs)

getTableFromOne :: FilePath -> IO Table
getTableFromOne topDir = do
    paths <- getRecursivePaths topDir
    mapM (getFileMeta (cPath topDir ++ "/")) paths

getRecursivePaths :: FilePath -> IO [FilePath]
getRecursivePaths topDir = do
    paths' <- getDirectoryContents topDir
    let paths'' = filter (`notElem` [".", ".."]) paths'
    let paths = map (topDir </>) paths''
    dirs <- pickoutDirs paths
    liftM (paths++) (liftM concat (forM dirs getRecursivePaths))

pickoutDirs :: [FilePath] -> IO [FilePath]
pickoutDirs [] = return []
pickoutDirs (x:xs) = do
    isDir <- doesDirectoryExist x
    if isDir
    then liftM ([x]++) (pickoutDirs xs)
    else pickoutDirs xs
    
getFileMeta :: FilePath -> FilePath -> IO Row
getFileMeta topDir fPath = do
    fStatus <- getFileStatus fPath
    let cononPath' = cPath fPath
    let cononPath = stripPrefix' (cPath topDir ++ "/") cononPath'
          where stripPrefix' pre str =
                  case stripPrefix pre str of
                    Nothing -> str
                    Just x -> x
    let (fullName:baseName:extension:[]) = getNames cononPath (isDirectory fStatus)
    atime <- epochToLocaleHuman ( (read $ show $ accessTime fStatus) :: Integer       )
    mtime <- epochToLocaleHuman ( (read $ show $ modificationTime fStatus) :: Integer )
    ctime <- epochToLocaleHuman ( (read $ show $ statusChangeTime fStatus) :: Integer )
    let r = [ ("path", cononPath)
            , ("fullname", fullName)
            , ("basename", baseName)
            , ("extension", extension)
            , ("depth", getDepth cononPath)
            , ("type", getType fStatus)
            , ("size", show (fileSize fStatus) ++ "B")
            , ("atime", atime)
            , ("mtime", mtime)
            , ("ctime", ctime)
            ]
    return r

getType :: FileStatus -> String
getType fStatus
    | isRegularFile fStatus = "f"
    | isDirectory fStatus = "d"
    | isSymbolicLink fStatus ="s"
    | otherwise = "<unknown>"

type IsDir = Bool
getNames :: FilePath -> IsDir -> [String]
getNames fPath True = map cPath [fullName, baseName, ""]
    where fullName = baseName
          baseName = takeBaseName . cPath $ fPath
getNames fPath False = map cPath [fullName, baseName, extension]
    where fullName = baseName ++ case extension of
                                   "" -> ""
                                   x -> "." ++ x
          baseName = takeBaseName . cPath $ fPath
          extension = case takeExtension . cPath $ fPath of
                        ('.':xs) -> xs
                        x -> x
                        

getDepth :: FilePath -> String
getDepth "" = "0"
getDepth (x:xs)
    | x == '/' = show $ 1 + ((read $ getDepth xs) :: Int)
    | otherwise = getDepth xs


-- replace back slashes with forward slashes
-- remove trailing forward slashes
cPath = canonicalizePath
canonicalizePath :: FilePath -> FilePath
canonicalizePath = stripR . (replace "\\" "/") . (replace "\\\\" "/")
    where stripR str = if "/" `isSuffixOf` str
                       then stripR $ init str
                       else str
