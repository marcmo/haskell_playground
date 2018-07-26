module ProjectInfo.ProjectParser where

import Monad(liftM)
import ProjectInfo.Cproject
import ProjectInfo.Parsing(parseProjectName)
import Maybe(fromJust,isJust)
import Text.XML.HaXml
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Util(docContent)
import Text.XML.HaXml.Html.Generate
import Text.XML.HaXml.XmlContent.Haskell
import System.FilePath(splitFileName,dropTrailingPathSeparator)
import Data.List(nub,intersect)
import Data.Either(rights)
import Control.Monad.Reader

type ProjectEnvFunc = Reader [CProject]

consolidateDependencies :: CProject -> ProjectEnvFunc ProjectDependency
consolidateDependencies p = do
  ps <- ask 
  let validNames = map projectName ps
  let listedProjects = extractProjects $ concatMap includePaths (configs p)
  return $ ProjectDependency 
            (projectName p) (intersect listedProjects validNames) 

dependencies :: CProject -> ProjectEnvFunc ProjectDependency
dependencies p = do
  deps <- getDependencies p
  return $ ProjectDependency (projectName p) deps

getDependencies ::  CProject -> ProjectEnvFunc [String]
getDependencies p = do
  ps <- ask
  return $ extractProjects $ concatMap includePaths (configs p)

extractProjects ::  [String] -> [String]
extractProjects ps = 
  nub $ rights $ map parseProjectName ps

getContent :: Document Posn -> Content Posn 
getContent (Document _ _ e _) = CElem e noPos

getConfigurations :: Document Posn -> [Configuration]
getConfigurations doc = map createConfig contents
    where contents = (configFilter . getContent) doc
          createConfig :: Content Posn -> Configuration
          createConfig c = 
            Configuration (head $ configName c) (getIncludePaths c)

configName :: Content Posn -> [String]
configName c = 
  let as = getAttributes c
      xs = map (fromAttrToStr "name") as in
    map fromJust $ filter isJust xs      

parseProjectDesc :: FilePath -> IO CProject
parseProjectDesc f = do
  inpStr <- readFile f
  let cfgs = getConfigurations $ xmlParse "unknown" inpStr
  return $ CProject { absolutPath = dropTrailingPathSeparator . fst $ splitFileName f,
                    projectName = extractProjectName f,
                    configs = cfgs }

extractProjectName f = snd $ (splitFileName . dropTrailingPathSeparator . fst) $ splitFileName f

getIncludePaths :: Content Posn -> [String]
getIncludePaths c = 
  case getDiabCompilerSetting c of
    Nothing -> []
    Just compilerSettings ->              
      let ips = includePathFilter compilerSettings
          as = map getAttributes ips
          xs = concatMap (map (fromAttrToStr "value")) as in
        map fromJust $ filter isJust xs
 
getDiabCompilerSetting :: Content Posn -> Maybe (Content Posn)
getDiabCompilerSetting c =
  let settings = cppCompiler c in
    if length settings > 0 then Just $ head settings else Nothing

-- cfilters
configFilter = (deep $ tag "configuration")
cppCompiler = ((attrval ("name",AttValue[Left "Diab C++ Compiler"])) `o` (deep  (tag "tool")))
includePathFilter =
    (deep (tag "listOptionValue") `o` (tag "option" `o` (deep ipath)))
  where ipath =  tag "tool" /> (attrval ("name",AttValue[Left "Include paths (-I)"]))
ipf = attrval ("name",AttValue[Left "Include paths (-I)"]) `o` (deep  (tag "option"))

getAttributes :: Content Posn -> [Attribute]
getAttributes (CElem (Elem name attributes _) _ ) = attributes
getAttributes _ = []

contentToStringDefault :: String -> [Content ()] -> String
contentToStringDefault msg [] = msg
contentToStringDefault _ x = contentToString x

contentToString :: [Content ()] -> String
contentToString = 
    concatMap procContent
    where procContent x = 
              verbatim $ keep /> txt $ CElem (unesc (fakeElem x)) ()

          fakeElem :: Content () -> Element ()
          fakeElem x = Elem "fake" [] [x]

          unesc :: Element () -> Element ()
          unesc = xmlUnEscape stdXmlEscaper
