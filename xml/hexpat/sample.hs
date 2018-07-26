import Text.XML.Expat.Tree
import Text.XML.Expat.Format
import Text.XML.Expat.Proc
import System.Environment
import System.FilePath
import System.Exit
import System.IO
import SearchFiles(listMatches)
import Control.Monad
import Control.Exception
import qualified Data.ByteString.Lazy as L
 
projectPath="/home/omueller/work/arxml_files_fem_ls9/"
xmlPath = "/home/omueller/dev/git/FEM/parameters_and_diagnosis/fem/diagnosis/container/"

main = do 
    -- xmlFiles <- listMatches projectPath "*.arxml"
    xmlFiles <- listMatches xmlPath "*.odx-d"
    print $ length xmlFiles
    mapM_ print xmlFiles
    -- print $ projectPath ++ (show $ length xmlFiles)
    mapM_ (process printOutAllTags) xmlFiles

main2 = do
    args <- getArgs
    case args of
        [filename] -> process printOutAllTags filename
        otherwise  -> do
            hPutStrLn stderr "Usage: helloworld <file.xml>"
            exitWith $ ExitFailure 1
 
process ::  (Handle -> UNode String -> IO ()) -> FilePath -> IO ()
process f filename = do
    inputText <- L.readFile filename
    let (xml, mErr) = parse defaultParserOptions inputText :: (UNode String, Maybe XMLParseError)
    let elems = filterElements sampleFilter xml
    -- when (length elems > 0) $
    --     bracket (openFile ("/home/omueller/tmp/gen/" ++ takeFileName filename ++ ".out") WriteMode)
    --             (hClose) 
    --             (\h -> (f h xml))
    -- mapM_ printNodeInfo elems
    print $ length elems
    case mErr of
        Nothing -> return ()
        Just err -> do
            hPutStrLn stderr $ "XML parse failed: "++show err
            exitWith $ ExitFailure 2

sampleFilter :: GenericXMLString tag => Node tag text -> Bool
sampleFilter (Element n as cs) = 
  gxToString n == "DTC"

printNodeInfo :: UNode String -> IO ()
printNodeInfo n@(Element name attributes childs) = do
  putStrLn name
  let ds = filterElements filterDataElements n
  let display = filterElement displayFilter n
  print attributes
  mapM_ printDE ds
  maybe (return ()) print display

displayFilter (Element n as cs) = gxToString "DISPLAY-TROUBLE-CODE" == n
printDE (Element n as cs) = do
  putStrLn $ "  ----> " ++ n ++ "  " ++ (show $ length cs)

filterDataElements :: GenericXMLString tag => Node tag text -> Bool
filterDataElements (Element n as cs) =
  gxToString n == "DATA-ELEMENTS"

printOutAllTags :: Handle -> UNode String -> IO ()
printOutAllTags h (Text t) = hPutStrLn h "was text"
printOutAllTags h (Element n as cs)  = do
  hPrint h n
  mapM_ (hPrint h) as
  mapM_ (printOutAllTags h) cs
