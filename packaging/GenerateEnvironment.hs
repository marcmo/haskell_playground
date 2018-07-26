import Test.QuickCheck
import Test.QuickCheck.Property
import PackageSystem 
import Data.List
import System
import System.Directory
import System.FilePath
import Control.Monad
import System.IO
import Text.PrettyPrint
import Config
import Control.Applicative

type PackageInfo = (PackageId,Version)

testSetup ::  Int -> IO ()
testSetup n = do
  s <- sample' (genEnv n)
  mapM_ setupRepo (head s)

setupRepo ::  SoftwarePackage -> IO ()
setupRepo (MkPackage id v deps) = do
  let installDest = remoteRepo </> id
  putStrLn installDest
  createDirectoryIfMissing True installDest
  withinDir installDest $ do
    systemDebug ("git init")
    createDistFile id v deps
    systemDebug ("git add .")
    systemDebug ("git ci -m'initial commit'")
    systemDebug ("git tag " ++ show v)

createDistFile ::  String -> Version -> [Dependency] -> IO ()
createDistFile n v ds = do
    let nameDoc = text "Name:" $$ nest 15 (text n)
    let versionDoc = text "Version:" $$ nest 15 (text $ show v)
    let libDoc = text "Library"
    let buildDeps = nest 15 (text "    Build-Depends:    ")
    let libs = hcat $ punctuate comma (map (text . show) ds)
    outh <- openFile (n ++ ".dispac") WriteMode
    hPutStrLn outh $ (render . hcat . punctuate (text "\n")) [nameDoc,versionDoc,libDoc,buildDeps <> libs]
    hClose outh
  
genEnv :: Int -> Gen [SoftwarePackage]
genEnv n = do
  pInfos <- genPackageInfos n
  forM pInfos $ \i -> genPackage i (delete i pInfos)

genPackage :: PackageInfo -> [PackageInfo] -> Gen SoftwarePackage
genPackage (id,version) xs = do
  p_deps <- genDeps xs
  let deps = map (\(x,y)->Dep x y) p_deps
  return $ MkPackage id version deps

genName = do
  len <- choose (3,10)
  vectorOf len (elements ['a'..'z'])

genPackageInfos :: Int -> Gen [PackageInfo]
genPackageInfos n = do
  count <- choose (1,n)
  names <- replicateM count genName
  versions <- vectorOf count genVersion
  return $ zip names versions

genVersion :: Gen Version
genVersion = Version <$> v <*> v <*> v
    where v = choose (0,9)

genDeps :: [PackageInfo] -> Gen [PackageInfo]
genDeps ids = take <$> choose (0,upper) <*> permutation ids
    where upper = truncate $ (fromIntegral $ length ids)/2

permutation :: Eq a => [a] -> Gen [a]
permutation initial = inner initial []
  where
    inner :: Eq a => [a] -> [a] -> Gen [a]
    inner [] accum = return accum
    inner xs accum = do
      p <- choose(0,(length xs)-1)
      inner (delete (xs!!p) xs) ((xs!!p):accum)

systemDebug s = putStrLn s >> system s

withinDir ::  FilePath -> IO a -> IO ()
withinDir d a = do
  curDir <- getCurrentDirectory
  setCurrentDirectory d >> a >> setCurrentDirectory curDir
