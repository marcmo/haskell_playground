
import System.Environment(getArgs,getProgName)
import System.Directory
import System.FilePath
import Control.Applicative((<$>))
import Text.Printf(printf)
import Data.Time.Clock(utctDay,getCurrentTime)
import Data.Time.Calendar(toGregorian)
import qualified Data.Map as M
import Control.Monad.State
import Text.StringTemplate
import Data.Maybe(fromMaybe)
import Data.Char(isUpper,isLower,toUpper)
import Data.List(stripPrefix,nub)

type Dict = StateT (M.Map String String) IO

addToDict :: String -> String -> Dict ()
addToDict key val = modify (M.insert key val)

getValue :: String -> Dict (Maybe String)
getValue key = gets (M.lookup key)

-- Tries to get value from dictionary, if fails,
-- asks user to enter value and store it in the map
queryValue :: String -> Dict String
queryValue key = do
  mv <- getValue key
  case mv of
    Just v -> return v
    Nothing -> do
      liftIO $ print $ "no mapping found for: " ++ breakWord key ++ ", please enter: "
      val <- liftIO getLine
      -- val <- fromMaybe "" <$> (liftIO $ readline $ printf "%s: " (breakWord key))
      addToDict key val
      return val

-- Breaks CamelCaseIdentifiers to separate words
breakWord (x:y:xs) | isLower x && isUpper y = x:' ':y:breakWord xs
                   | y == '_' = x:' ':breakWord xs
                   | otherwise = x : breakWord (y:xs)
breakWord xs = xs

main :: IO ()
main = do
  args <- getArgs
  (tmpl, name, contestId) <- case args of
    [tmpl,name,contestId] -> return (tmpl, name, contestId)
    [tmpl,name]           -> return (tmpl, name, "")
    _                     -> do
                 prgName <- getProgName
                 error $ "Usage: " ++ prgName ++ " template_name project_name"
  initProject tmpl name contestId

-- returns year, month, day
today = toGregorian . utctDay <$> getCurrentTime

ifM :: (Monad m) => m Bool -> m c -> m c -> m c
ifM a b c = do
  r <- a
  if r then b else c

failWhenM cv errString = ifM cv (error errString) (return ())

initProject ::  FilePath -> FilePath -> String -> IO ()
initProject tmpl name contestId = do
  let (prjPath, prjName) = splitFileName name
  failWhenM (not <$> doesDirectoryExist tmpl)
    (printf "Couldn't find template '%s'" tmpl)
  failWhenM (doesDirectoryExist name)
    (printf "Project with name '%s' exists in folder '%s'" prjName prjPath)
  (y,m,d) <- today
  let mapping = M.fromList [("ProjectName", prjName),
                        ("ModuleName", ap ((:) . toUpper . head) tail prjName),
                        ("ContestId", contestId),
                        ("Year", show y),
                        ("Month", printf "%02d" m),
                        ("Day", printf "%02d" d)]
  -- of course this directory doesn't exist yet, but
  -- only this function allows us to create directory with
  -- parents
  createDirectoryIfMissing True name
  runStateT (applyTemplate tmpl name) mapping
  return ()

applyTemplate :: FilePath -> FilePath -> Dict ()
applyTemplate fromDir toDir = do
  -- recursively find directories and files, and copy them.
  contents <- filter (`notElem` [".", ".."]) <$>  liftIO (getDirectoryContents fromDir)
  files <- liftIO $ filterM (doesFileExist . (fromDir </>)) contents
  dirs <- liftIO $ filterM (doesDirectoryExist . (fromDir </>)) contents
  mapM_ (cpDir fromDir toDir) dirs
  mapM_ (cpFile fromDir toDir) files

cpDir :: FilePath-> FilePath -> FilePath -> Dict ()
cpDir from to dirname = do
  toName <- transformName dirname
  liftIO $ createDirectory (to </> toName)
  applyTemplate (from </> dirname) (to </> toName)

cpFile :: FilePath-> FilePath -> FilePath -> Dict ()
cpFile from to filename = do
  toName <- transformName filename
  if takeExtension toName == ".template"
    then liftIO (readFile (from </> filename)) >>=
         fillTemplate >>=
         liftIO . writeFile (to </> dropExtension toName)
    else liftIO $ copyFile (from </> filename) (to </> toName)

-- copy the template file. Input arguments - complete filenames
fillTemplate ::  String -> Dict String
fillTemplate strIn = do
  let tmpl = newSTMP strIn :: StringTemplate String
      (_, vars', _) = checkTemplate tmpl
      vars = fromMaybe [] vars'
  -- reverse vars, to force them appear in normal order
  vals <- mapM (\a -> (,) a <$>  queryValue a) $ nub $ reverse vars
  return $ render $ setManyNativeAttrib vals tmpl

-- when filename starts with '__', than rename file to the dictionary value
transformName :: FilePath -> Dict FilePath
transformName filename = maybe (return filename)
      (liftM (<.> extensions) . queryValue)
      (stripPrefix "__" name)
  where extensions =  takeExtensions filename
        name = dropExtensions filename

