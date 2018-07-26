module Util.IncludeFinder
  (
    getIncludeList
    , IncludePaths(..)
    , StringSet
  ) where

import ProjectInfo.Parsing(extractIncludePath)
import Util.SearchFiles(listMatches)
import Control.Monad.Reader(foldM)
import Data.Either(rights)
import qualified Data.Set as Set
import qualified System.IO.Strict as Strict
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))
import qualified Data.ByteString as Str
import Data.ByteString.Char8(lines)
import Prelude hiding(lines)


type StringSet = Set.Set String  

data IncludePaths = IP { project :: String,
                          includePaths :: StringSet } deriving (Show)

getIncludeList ::  FilePath -> IO IncludePaths
getIncludeList projectPath = do
  includes <- includeLinesInProject projectPath
  return $ IP (snd $ splitFileName projectPath) includes

combineWith :: (FilePath -> IO StringSet) -> StringSet -> FilePath -> IO StringSet
combineWith f oldSet p = do
  newSet <- f p
  return $ Set.union oldSet newSet

includeLinesInProject ::  FilePath -> IO StringSet
includeLinesInProject projectPath = do
  headers <- listMatches projectPath "*.h"
  cppfiles <- listMatches projectPath "*.cpp"
  allLines <- foldM (combineWith includeLines) Set.empty (headers ++ cppfiles)
  return $ allLines
  
includeLines ::  FilePath -> IO StringSet
includeLines file = do
  content <- Str.readFile file -- read as bytestring to avoid encoding problems
  let matches = map extractIncludePath (lines content)
  return $ Set.fromList (rights matches)

