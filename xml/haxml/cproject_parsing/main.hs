module Main  where

import qualified ProjectInfo.Cproject as Cproject
import Util.IncludeFinder(IncludePaths,StringSet,getIncludeList,includePaths,project)
import ProjectInfo.ProjectParser(parseProjectDesc,consolidateDependencies)
import System(getArgs)
import Util.SearchFiles(listMatches,existsInFolder)
import Control.Monad.Reader(runReader)
import ProjectInfo.Graph(createDependencyGraph)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))
import System.Directory(doesFileExist)
import Monad(foldM)

type IncludeLineMap = Map.Map String StringSet
type HeaderMap = Map.Map String StringSet

main = do 
  (n:workspacePath:_) <- getArgs
  projectFiles <- listMatches workspacePath "*cproject"
  projects <- mapM parseProjectDesc projectFiles
  let projectNames = map Cproject.projectName projects
  let projectDirs = [workspacePath </> n | n <- projectNames]
  includeLists <- mapM getIncludeList projectDirs -- [IncludePaths]
  let includeLineMap = foldl mergeMaps Map.empty includeLists
  let getProjectDeps p = runReader (consolidateDependencies p) projects
  let allDependencies = map getProjectDeps projects
  headerMap <- foldM (add2map workspacePath) Map.empty projectNames
  print headerMap
  let directDependencies = map (removeIndirectDeps headerMap includeLineMap) allDependencies
  case n of 
    "1" -> putStrLn $ unlines $ map show projects 
    "2" -> writeFile "output.txt" $ unlines $ map show directDependencies
    "3" -> writeFile "output.txt" $ show includeLineMap
    "4" -> writeFile "output.dot" (createDependencyGraph directDependencies)

removeIndirectDeps :: HeaderMap -> IncludeLineMap -> Cproject.ProjectDependency -> Cproject.ProjectDependency
removeIndirectDeps headerMap lineMap dep@(Cproject.ProjectDependency name dependents) =
  case Map.lookup (Cproject.getProject dep) lineMap of
    Nothing -> dep -- should not happen. we cannot exclude dependencies when include lines are not available for project
    Just lines -> Cproject.ProjectDependency name direct
      where direct = filter (hasSthFromIncludeLines headerMap lines) dependents

hasSthFromIncludeLines :: HeaderMap -> StringSet -> String -> Bool
hasSthFromIncludeLines headerMap lines project =
  let (Just headerSet) = Map.lookup project headerMap
      existsInProject line = Set.member (snd $ splitFileName line) headerSet in
    any existsInProject (Set.toList lines)

mergeMaps :: IncludeLineMap -> IncludePaths -> IncludeLineMap
mergeMaps acc ips = Map.insert (project ips)(includePaths ips) acc

add2map :: FilePath -> HeaderMap -> String -> IO HeaderMap
add2map workspacePath headers p = do
  newHeaders <- listMatches (workspacePath </> p) "*.h"
  let namesOnly = map (snd . splitFileName) newHeaders
  return $ Map.insert p (Set.fromList namesOnly) headers
