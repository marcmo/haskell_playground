module VerifyCompatiblityBlocks
where

import NvramCompatibility
import Nvram
import Control.Monad.Reader
import Data.List(find)
import Maybe(isJust,fromJust)
import Debug.Trace
trc xs = trace (concat xs) -- debug on
-- trc xs a = a -- debug off

type NvramEnv a = Reader [Block] a

checkNoSecret :: NvramCompatibilityConfig -> NvramConfig -> [CompatibilityBlock]
checkNoSecret (MkCompConfig cbs) (NvramConfig [(Swc _ blocks)]) = 
  filter (\x-> runReader (isSecret x) blocks) cbs

checkCorrectOffsets :: NvramCompatibilityConfig -> [(CompatibilityBlock,CompatibilityBlock)]
checkCorrectOffsets (MkCompConfig cbs) =
  let pairs = zip cbs (tail cbs) in
    filter (\(a,b)-> start a + blockLength a /= start b) pairs

checkCorrectLength :: NvramCompatibilityConfig -> NvramConfig -> [CompatibilityBlock]
checkCorrectLength (MkCompConfig cbs) (NvramConfig [(Swc _ blocks)]) =
  filter (\x-> not $ runReader (hasSameLength x) blocks) cbs

compareLength :: NvramCompatibilityConfig-> NvramConfig-> [(String, (Int, Int))]
compareLength (MkCompConfig cbs) (NvramConfig [(Swc _ blocks)]) =
  map (\x-> (name x,runReader (hasSameLengthDebug x) blocks)) cbs

isSecret :: CompatibilityBlock -> NvramEnv Bool
isSecret (CB n _ _) = do
  currentBlock <- findBlock n 
  return $
    maybe False secret currentBlock

hasSameLength :: CompatibilityBlock -> NvramEnv Bool
hasSameLength (CB n _ len) = do
  currentBlock <- findBlock n
  return $
    maybe (False) ((len ==) . layoutSize) currentBlock

hasSameLengthDebug ::  CompatibilityBlock -> Reader [Block] (Int, Int)
hasSameLengthDebug (CB n _ len) = do
  currentBlock <- findBlock n
  return $
    maybe (0,0) (\x-> (layoutSize x,len)) currentBlock

layoutSize ::  Block -> Int
layoutSize b = 
  let blockSize = if (isJust . checksum) b then size b + 2 else size b in
    if (isJust . datasetSize) b then blockSize * (fromJust . datasetSize) b else blockSize


findBlock :: String -> NvramEnv (Maybe Block)
findBlock n = do
  blocks <- ask
  return $ find (\x->n == bName x) blocks


