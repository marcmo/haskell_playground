module SearchFiles where

import Control.OldException (handle)
import Control.Monad (forM)
import GlobRegex (matchesGlob)
import System.Directory (doesDirectoryExist, 
                         getCurrentDirectory, getDirectoryContents)
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = {-# SCC "recursiveContent" #-} do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
    dirName' <- if null dirName
                then getCurrentDirectory
                else return dirName
    handle (const (return [])) $ do
        names <- getRecursiveContents dirName'
        return (filter (`matchesGlob` pat) names)
      
existsInFolder :: FilePath -> String -> IO Bool
existsInFolder folder fileName = do
  matches <- listMatches folder ("*" ++ fileName)
  return $ length matches > 0
