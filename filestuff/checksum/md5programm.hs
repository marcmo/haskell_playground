import Control.Monad
import Data.Digest.Pure.MD5 (md5)
import Data.List (sort)
import System.Directory
import System.FilePath
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = mapM_ (\path -> putStrLn . fileLine path =<< BS.readFile path)
   =<< getRecursiveContents "."

fileLine :: FilePath -> BS.ByteString -> String
fileLine path c = hash c ++ " " ++ path

hash :: BS.ByteString -> String
hash = show . md5

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
	names <- getDirectoryContents topdir
	let properNames = filter (`notElem` [".", ".."]) names
	paths <- concatForM properNames $ \name -> do
		let path = topdir </> name
		isDirectory <- doesDirectoryExist path
		if isDirectory
			then getRecursiveContents path
			else do
				isFile <- doesFileExist path
				return [path | isFile]
	return (sort paths)

concatForM :: (Monad m) => [a1] -> (a1 -> m [a]) -> m [a]
concatForM xs f = liftM concat (forM xs f)




