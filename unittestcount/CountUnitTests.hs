module CountUnitTests where

import Maybe(fromJust)
import Control.Monad (forM,filterM,foldM,liftM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.Environment 
import Text.Regex.Posix
import Text.Regex
import Data.Char(toUpper)
import System.IO
import System.IO.Unsafe
import qualified Data.ByteString.Lazy.Char8 as B

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

getFirstLevelDirectories :: IO [FilePath]
getFirstLevelDirectories  = do
  names <- getDirectoryContents "."
  let properNames = filter pIsValid names
  paths <- forM properNames $ \name -> do
    let path = "." </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then return [path]
      else return []
  return (concat paths)

isSourceFile :: FilePath -> Bool
isSourceFile = (=~ "\\.(h|cpp|c|hpp)$")
getFiles :: FilePath -> IO [FilePath]
getFiles topdir = getRecursiveContents pIsValid topdir


pIsValid = (not . (`elem` [".", "..",".metadata"]))
f x y =  x =~ y
pIsValid2 :: [Char] -> Bool
pIsValid2  = (not . (=~ "^\\."))

getSourceFiles :: FilePath -> IO (FilePath,[FilePath])
getSourceFiles topdir = do
  fs <- getFiles topdir
  return (topdir,filter isSourceFile fs)

countLines :: FilePath -> IO (FilePath, Int)
countLines f = do
  inf <- readFile f
  return (f, (length . (filter (not . pCommentOrEmpty)) . lines) inf)

pCommentOrEmpty :: String -> Bool
pCommentOrEmpty  = (=~ "^[ ]*(//|/\\*|$)")

main2 = do
  dirs <- getFirstLevelDirectories
  mapM putStrLn dirs
main4 = do
  filteredDirs <- getFirstLevelDirectories
  srcFiles2 <- mapM getSourceFiles filteredDirs  :: IO [(FilePath,[FilePath])]
  mapM printInfo srcFiles2

unsafeInterleaveMapIO f (x:xs) = 
    unsafeInterleaveIO $ do
      y <- f x
      ys <- unsafeInterleaveMapIO f xs
      return (y : ys)
unsafeInterleaveMapIO _ [] = return []

getLineInfo :: [FilePath] -> IO [(FilePath, Int)]
getLineInfo xs = do
  li <- unsafeInterleaveMapIO countLines xs
  return li

printInfo :: (String, [FilePath]) -> IO ()
printInfo (top,xs) = do
  lineInfo <- getLineInfo xs
  let total = foldr (\(_,y) x->x+y) 0 lineInfo
  if (total > 0) 
    then
      putStrLn (top ++ "," ++ (show $ length lineInfo) ++ "," ++ show total) 
    else 
      return ()

-- filterFile :: (String, [FilePath]) -> IO ()
-- filterFile (top,xs) = do
  
isUnitTest :: FilePath -> Bool
isUnitTest = (=~ "CPPUNIT_TEST\\(")

main = do
  name:out:_ <- getArgs
  file <- readFile name
  let testLines = filter isUnitTest $ lines file
  mapM (\x-> appendFile out (x ++ "\n")) testLines
 
