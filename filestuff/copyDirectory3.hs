import System.Directory
import System.FilePath
import Control.Applicative
import Control.Exception(throw)
import Control.Monad
import System.IO.HVFS
import System.Path

copyDir3 ::  FilePath -> FilePath -> IO ()
copyDir3 src dst = do
  content <- recurseDir SystemFS src
  -- dirs <- filterM doesDirectoryExist content
  -- files <- filterM doesFileExist content
  (dirs,files) <- foldM dirsAndFiles ([],[]) content
  forM_ dirs $ \d -> createDirectory d
  forM_ files $ \f -> copyFile f f 


dirsAndFiles (dirs,files) x = do
  isdir <- doesDirectoryExist x
  return $ if isdir
      then ((x:dirs),files)
      else (dirs,(x:files))

