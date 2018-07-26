module Util.Unsafe

where

import Util.SearchFiles
import qualified Data.Set as Set
import qualified System.IO.Strict as Strict
import Data.Either(rights)
import ProjectInfo.Parsing(extractIncludePath)
import System.IO.Unsafe

type StringSet = Set.Set String  

includeContainingFiles ::  FilePath -> IO [StringSet]
includeContainingFiles f = do
  headers <- listMatches f "*.h"
  cppfiles <- listMatches f "*.cpp"
  allLines <- unsafeInterleaveMapIO includeLines (headers ++ cppfiles)
  return $ allLines
  
includeLines ::  FilePath -> IO StringSet
includeLines file = do
  content <- Strict.readFile file
  let matches = rights $ map extractIncludePath (lines content)
  return $ Set.fromList matches

unsafeInterleaveMapIO f (x:xs) = unsafeInterleaveIO $ do
  y <- f x
  ys <- unsafeInterleaveMapIO f xs
  return (y : ys)
unsafeInterleaveMapIO _ [] = return []

