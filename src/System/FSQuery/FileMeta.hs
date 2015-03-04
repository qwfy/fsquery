module System.FSQuery.FileMeta
    ( getTableFromOne
    ) where


import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Data.List (nub, (\\), stripPrefix, isSuffixOf)
import Control.Monad (mapM, liftM)
import Data.Maybe (fromMaybe)

import Data.String.Utils (replace, split)

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

import System.FSQuery.Data
import System.FSQuery.UnitConvert

getTableFromOne :: SourceSpec -> IO Table
getTableFromOne spec@(topDir, depth) = do
    paths <- getRecursivePaths spec
    mapM (getFileMeta (cPath topDir ++ "/")) paths

getRecursivePaths :: SourceSpec -> IO [FilePath]
getRecursivePaths (topDir, Just (-1)) = return []
getRecursivePaths (topDir, depth) = do
    paths <- getFileList topDir
    dirs <- pickoutDirs paths
    let d = fmap (subtract 1) depth
    let specs = [(x, d) | x <- dirs]
    liftM ((paths++) . concat) $ mapM getRecursivePaths specs

getFileList :: FilePath -> IO [FilePath]
getFileList topDir = do
    paths' <- getDirectoryContents topDir
    let paths'' = filter (`notElem` [".", ".."]) paths'
    return $ map (topDir </>) paths''

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
                  fromMaybe str $ stripPrefix pre str
    atime <- epochToLocaleHuman ( (read $ show $ accessTime fStatus) :: Integer       )
    mtime <- epochToLocaleHuman ( (read $ show $ modificationTime fStatus) :: Integer )
    ctime <- epochToLocaleHuman ( (read $ show $ statusChangeTime fStatus) :: Integer )
    return [ ("path", cononPath)
           , ("name", getName cononPath)
           , ("basename", getBaseName cononPath)
           , ("extension", getExtension cononPath)
           , ("depth", getDepth cononPath)
           , ("type", getType fStatus)
           , ("size", show (fileSize fStatus) ++ "B")
           , ("atime", atime)
           , ("mtime", mtime)
           , ("ctime", ctime)
           ]

getType :: FileStatus -> String
getType fStatus
    | isRegularFile fStatus = "f"
    | isDirectory fStatus = "d"
    | isSymbolicLink fStatus ="s"
    | otherwise = "<unknown>"

getName :: FilePath -> FilePath
getName = last . split "/" . cPath

getBaseName :: FilePath -> FilePath
getBaseName x =
    case getName x of
      ('.':rest) -> '.' : head (split "." rest)
      y -> head $ split "." y

getExtension :: FilePath -> FilePath
getExtension x =
    let p = case getName x of
              ('.':rest) -> rest
              y -> y
    in case split "." p of
         [z] -> ""
         w -> last w


getDepth :: FilePath -> String
getDepth "" = "0"
getDepth (x:xs)
    | x == '/' = show $ 1 + ((read $ getDepth xs) :: Int)
    | otherwise = getDepth xs


-- replace back slashes with forward slashes
-- remove trailing forward slashes
cPath = canonicalizePath
canonicalizePath :: FilePath -> FilePath
canonicalizePath = stripR . replace "\\" "/" . replace "\\\\" "/"
    where stripR str = if "/" `isSuffixOf` str
                       then stripR $ init str
                       else str
