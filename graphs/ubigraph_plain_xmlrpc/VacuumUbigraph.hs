module VacuumUbigraph where

import GHC.Vacuum
import Data.Char
import Text.Printf
import Data.List

import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import qualified Ubigraph as U

nodeStyle n =
    case nodeName n of
      ":"  -> ("(:)", "cube", "#0000ff")

      -- atomic stuff is special
      k | k `elem` ["S#" ,"I#" ,"W#"
                   ,"I8#" ,"I16#" ,"I32#" ,"I64#"
                   ,"W8#" ,"W16#" ,"W32#" ,"W64#"] -> (showLit n, "sphere", "#00ff00")
      -- chars
      "C#" -> (show . chr . fromIntegral . head . nodeLits $ n, "sphere", "#00ff00")
      "D#" -> ("Double", "sphere", "#009900")
      "F#" -> ("Float", "sphere", "#009900")

      -- bytestrings
      "PS"    -> (printf "ByteString[%d,%d]" (nodeLits n !! 1) (nodeLits n !! 2), "cube", "#ff0000")
      "Chunk" -> (printf "Chunk[%d,%d]" (nodeLits n !! 1) (nodeLits n !! 2), "cube", "#ff0000")

      -- otherwise just the constructor and local fields
      c   | z > 0 ->
                    (c ++ show (take (fromIntegral z) $ nodeLits n), "cube", "#990000")
          | otherwise -> (c, "cube", "#990000")
                    where z = itabLits (nodeInfo n)
        where
          showLit n = show (head $ nodeLits n)

view a = do
  U.clear srv
  mapM_ renderNode nodes
  mapM_ renderEdge edges
  return g
    where      
      g = vacuum a
      alist = toAdjList g
      nodes = nub $ map fst alist ++ concatMap snd alist
      edges = concatMap (\(n, ns) -> map ((,) n) ns) alist

      style nid = maybe ("...", "cube", "#ff0000") nodeStyle (IntMap.lookup nid g)

      renderNode nid = do
           U.newVertexWithId srv nid
           let (label, shape, color) = style nid
           U.setVertexAttribute srv nid "label" label
           U.setVertexAttribute srv nid "shape" shape
           U.setVertexAttribute srv nid "color" color
      
      renderEdge (a, b) = do
           e <- U.newEdge srv a b
           U.setEdgeAttribute srv e "stroke" "dotted"
           U.setEdgeAttribute srv e "arrow" "true"

      srv = U.defaultServer

