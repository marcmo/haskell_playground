import System(getArgs)
import Control.Monad (forM,filterM)
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

main = do
  (d:[]) <- getArgs
  c <- getRecursiveContents d
  print $ length c


