module Coldground where

import Maybe(fromJust)
import Control.Monad (forM,filterM,liftM)
import System.Directory (doesDirectoryExist, getDirectoryContents, getCurrentDirectory)
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))
import Text.Regex.Posix
import Text.Regex
import Data.Char(toUpper)
import System.IO
import System( getArgs )
import qualified Data.Map as M

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

extractFileName :: String -> String
extractFileName =  head . fromJust . pictureMatch
pictureMatch = matchRegex (mkRegex "([a-zA-Z\\(\\)'+_]*)\\.(jpg|gif)")

isImageFile :: FilePath -> Bool
isImageFile = (=~ "\\.(gif|jpg)$")
getFiles :: FilePath -> IO [FilePath]
getFiles topdir = getRecursiveContents 
                       (not . (`elem` [".", ".."])) topdir

data ImageFolder = ImageFolder String [FilePath] 
                   deriving (Show)
imageFolders :: FilePath -> IO [ImageFolder]
imageFolders p = do
  content <- getDirectoryContents p
  folders <- filterM (doesDirectoryExist . (p </>)) content
  print $ p ++ " has appr. " ++ (show $ length folders) ++ " folders"
  let realfolders = filter (not . (`elem` [".", ".."])) folders
  mapM print realfolders
  tmp <- mapM (imageFolder . (p </>)) realfolders
  return (filter hasImages tmp)

hasImages :: ImageFolder -> Bool
hasImages (ImageFolder _ []) = False
hasImages _ = True
imageFolder :: FilePath -> IO ImageFolder
imageFolder path = do
  fs <- getFiles path
  let ifs = (filter isImageFile fs)
  return (ImageFolder path ifs)
  
getImageFiles :: FilePath -> IO [FilePath]
getImageFiles topdir = do
  fs <- getFiles topdir
  let ifs = (filter isImageFile fs)
  return ifs

type CountryMap = M.Map String [FilePath]

imageName :: String -> String
imageName xs =  let (_,n) = splitFileName xs in
                extractFileName n

type S3 = (String,String,String)
countryName :: String -> String
countryName (x:y:ys) = 
    let path = (dropTrailingPathSeparator . fst . splitFileName) ys 
        (_,name,_) = path =~ "[a-zA-Z0-9]*$" :: S3 
    in
      name
--writeImageFolder :: Handle -> ImageFolder -> IO ()
writeImageFolder outh (ImageFolder n pics) = do
  hPutStrLn outh (lightboxHtml True (head pics))
  mapM (\x-> hPutStrLn outh (lightboxHtml False x)) $ tail pics

lightboxHtml :: Bool -> String -> String
lightboxHtml full path  =
    let country = countryName path
        image = imageName path in
    if full then "<a href=\""++path++"\"rel=\"lightbox["++country++"]\"title=\""++image++"\"><img src=\""++path++"\" width=\"100\" height=\"40\" alt="++country++" class=\"bordered\" /></a>"
    else "<a href=\""++path++"\"rel=\"lightbox["++country++"]\"title=\""++image++"\"></a>"
main = do
  args <- getArgs
  case args of
    [imagepath,target] -> do
              inpStr <- readFile "src/page_start.txt"
              writeFile target inpStr
              iFolders <- imageFolders imagepath
              outh <- openFile target AppendMode
              mapM (writeImageFolder outh) iFolders
              inh <- openFile "src/page_end.txt" ReadMode
              mainloop inh outh
              hClose inh
              hClose outh
    _ -> print "usage: gen [image-directory][outputfile]"

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = 
    do ineof <- hIsEOF inh
       if ineof
          then return ()
          else do inpStr <- hGetLine inh
                  hPutStrLn outh inpStr
                  mainloop inh outh

