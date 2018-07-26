module RecursiveContents (getRecursiveContents) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Text.Regex.Posix

getRecursiveContents :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
getRecursiveContents p topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter p names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents p path
      else return [path]
  return (concat paths)

isImageFile :: FilePath -> Bool
isImageFile = (=~ "\\.(gif|jpg)$")
-- getImageFiles :: FilePath -> IO [FilePath]
-- getImageFiles topdir = getRecursiveContents 
--                        (and (=~ "\\.(gif|jpg)$") (not . (`elem` [".", ".."]))) topdir
getImageFiles topdir = getRecursiveContents isImageFile topdir

pred2 :: IO FilePath -> Bool
pred2 = undefined
-- getImageNames :: FilePath -> IO [FilePath]
-- getImageNames path = filter pred2  (getRecursiveContents path)
