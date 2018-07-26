import Test.QuickCheck
import Test.QuickCheck.Monadic(monadicIO,run,assert)
import System.Directory(removeFile,getTemporaryDirectory)
import System.IO
import Control.Exception(finally,bracket)

numCharactersInFile :: FilePath -> IO Int
numCharactersInFile fileName = do
    contents <- readFile fileName
    return (length contents)

numAlternative ::  FilePath -> IO Integer
numAlternative p = bracket (openFile p ReadMode) hClose hFileSize

data TestFile = TestFile String deriving (Eq,Ord,Show)
instance Arbitrary TestFile where
  arbitrary = do
    n <- choose (0,2000)
    testString <- vectorOf n $ elements ['a'..'z'] 
    return $ TestFile testString

prop_charsInFile (TestFile string) = 
  length string > 0 ==> monadicIO $ do
    (res,alternative) <- run $ createTmpFile string $
      \p h -> do
          alternative <- numAlternative p
          testRes <- numCharactersInFile p
          return (testRes,alternative)
    assert $ res == fromInteger alternative

createTmpFile :: String -> (FilePath -> Handle -> IO a) -> IO a
createTmpFile content func = do
       tempdir <- catch getTemporaryDirectory (\_ -> return ".")
       (tempfile, temph) <- openTempFile tempdir "" 
       hPutStr temph content
       hFlush temph
       hClose temph
       finally (func tempfile temph) 
               (removeFile tempfile)

