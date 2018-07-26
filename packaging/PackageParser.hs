import Distribution.Text
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import System
import System.Directory
import System.FilePath
import Control.Monad
import Maybe
import qualified Data.Set as S
import Distribution.Version
import Control.Monad.State
import Config
import UbigraphDisplayer
import Hubigraph
import Data.HashTable
import qualified PackageSystem as PS

packageFileEnding=".dispac"
type DependencySet = S.Set Dependency

instance Ord Dependency where
  (Dependency n1 v1) <= (Dependency n2 v2) = n1 <= n2

-- TODO global config with installed libs and repository path
-- TODO detect dependency discrepancies 
-- TODO list available packages
-- TODO list outdated packages
-- TODO provide self update

main = do
  contents <- getContents
  let res = parsePackageDescription contents
  print res

test f = do
  contents <- readFile f
  let (ParseOk _ pd) = parsePackageDescription contents
  let (Just x) = condLibrary  pd
  let deps = condTreeConstraints x
  print deps
  mapM_ (print . showDep) deps

testDownload ::  FilePath -> IO ()
testDownload f = do
  r $ clear
  evalStateT (downloadDeps f) S.empty
  
showDep ::  Dependency -> String
showDep (Dependency n vr) = show n ++ ", versions: " ++ show vr

status n = do
  knownDeps <- get
  io $ putStrLn $ "known deps so far:" ++ n
  io $ print knownDeps

getDeps :: FilePath-> StateT DependencySet IO (Maybe (String, PS.Version, [Dependency]))
getDeps fp = do
  status "getDeps"
  fs <- io $ findDispac fp
  if (null fs)
    then do
      io $ putStrLn $ "no dispac files for " ++ fp
      return Nothing
    else do
      io $ putStrLn $ "getDeps for " ++ fp
      contents <- io $ readFile $ fp </> (head fs)
      io $ putStrLn contents
      let (ParseOk _ pd) = parsePackageDescription contents
      let (Just x) = condLibrary pd
      let pId = package $ packageDescription pd
      let version = (mkVersion . versionBranch . pkgVersion) pId
      let (PackageName pN) = pkgName pId
      return $ Just (pN,version,condTreeConstraints x)

downloadDeps :: FilePath -> StateT DependencySet IO ()
downloadDeps fp = do
  status "downloadDeps"
  io $ putStrLn $ "downloadDeps for " ++ fp
  ds <- getDeps fp
  when (isJust ds) $ do
    let (name,version,newDeps) = fromJust ds
    io $ putStrLn $ "new dependencies for : " ++ name
    io $ print newDeps
    knownDeps <- get
    updateDependencies name version newDeps
    mapM_ downloadSingleDep (S.toList $ S.difference (S.fromList newDeps) knownDeps)

updateDependencies n v newDeps = do
    modify $ S.union (S.fromList newDeps)
    io $ updateGraph n v newDeps

toId n = fromIntegral ((hashString n) `mod` 200000000)
updateGraph :: String -> PS.Version -> [Dependency] -> IO ()
updateGraph n v ds = do
  let x = toId n
  putStrLn $ "updateGraph with " ++ show (n,x)
  r $ depictDeps n ds  x

depictDeps ::  String -> [Dependency] -> VertexID -> Hubigraph ()
depictDeps n ds vId = do newNode n vId
                         mapM_ build ds
  where build (Dependency (PackageName pName) version) = do
              let newId = toId pName
              newNode pName newId 
              newDep vId newId
              delay 200000

downloadSingleDep ::  Dependency -> StateT DependencySet IO ()
downloadSingleDep d = do
  status "downloadSingleDep"
  io $ putStrLn $ "trying to download " ++ (showDep d)
  let ((PackageName n),v) = depDetail d
  instDir <- io $ download v n
  paths <- io $ findDispac instDir
  when ((not . null) paths) $ downloadDeps instDir

downloadPackages ::  [(PackageName, PS.Version)] -> IO ()
downloadPackages ds =
  mapM_ (\((PackageName n),v)->download v n) ds

download :: PS.Version -> String -> IO FilePath
download version name = do 
  print $ "downloading..." ++ name ++ "," ++ show version
  let remote = remoteRepo </> name
  let installDir = localRepo </> name
  systemDebug ("git clone " ++ remote ++ " " ++ installDir)
  withinDir installDir $
    systemDebug ("git co -b tag_" ++ show version ++ " " ++ show version)
  return installDir

withinDir ::  FilePath -> IO a -> IO ()
withinDir d a = do
  curDir <- getCurrentDirectory
  setCurrentDirectory d
  a
  setCurrentDirectory curDir

systemDebug s = putStrLn s >> system s

depDetail ::  Dependency -> (PackageName, PS.Version)
depDetail (Dependency n vr) = (n,convertRange vr)
mkVersion [a,b,c] = PS.Version a  b  c
convertRange ::  VersionRange -> PS.Version
convertRange (ThisVersion v) =  mkVersion $ versionBranch v
-- convertRange ::  VersionRange -> [[Int]]
-- convertRange (AnyVersion) = []
-- convertRange (LaterVersion v) = [versionBranch v]
-- convertRange (EarlierVersion v) = [versionBranch v]
-- convertRange (UnionVersionRanges r1 r2) = convertRange r1 ++ convertRange r2
-- convertRange IntersectVersionRanges  VersionRange VersionRange

findDispac :: FilePath -> IO [FilePath]
findDispac path = getDirectoryContents path >>=
    return . filter (\p->takeExtension p == packageFileEnding)

io :: IO a -> StateT DependencySet IO a
io = liftIO

