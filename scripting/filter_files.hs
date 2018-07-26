{-# LANGUAGE NoMonomorphismRestriction #-}
import System.FilePath(takeExtension, dropExtension, (<.>), (</>))
import System.Directory(getDirectoryContents)
import Data.Char(isSpace)
import Control.Monad


getFilteredFiles :: IO [FilePath] -> (FilePath -> Bool) -> IO [FilePath]
getFilteredFiles files f = do
                xs <- files
                return $ filter f xs

filter2 :: Monad m => m a -> (a -> a) -> m a
-- filter2 :: IO [FilePath] -> ([FilePath] -> [FilePath]) -> IO [FilePath]
filter2 files f = do
        xs <- files
        let ys = f xs
        return ys
        
filter3 :: Monad m => m a -> (a -> a) -> m a
filter3 files f = liftM f $ files

filter4 :: Monad m => (a -> a) -> m a -> m a
filter4 = liftM

filterHs = liftM (filter isHaskellFile)

filterMyFiles = filter isHaskellFile

isHaskellFile :: FilePath -> Bool
isHaskellFile p = ".hs" == takeExtension p

-- myFilter :: IO [FilePath] -> (FilePath -> Bool) -> IO [FilePath]
myFilter = liftM filter
