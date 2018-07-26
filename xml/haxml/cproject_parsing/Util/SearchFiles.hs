{-# LANGUAGE ScopedTypeVariables #-}
module Util.SearchFiles where

import Control.Exception(IOException,handle)
import Control.Monad (forM)
import Util.GlobRegex (matchesGlob)
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
    handle (\(e :: IOException) -> putStrLn ("IOException occured!:" ++ show e) >> return []) $ do
        names <- getRecursiveContents dirName'
        return (filter (`matchesGlob` pat) names)

existsInFolder :: FilePath -> String -> IO Bool
existsInFolder folder fileName = do
  matches <- listMatches folder ("*" ++ fileName)
  return $ length matches > 0
