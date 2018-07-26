{-# LANGUAGE ScopedTypeVariables #-}
module GraphGenerator where
import Dijkstra
import GraphSerializer
import Pathfinder
import Data.List(nub,genericLength)
import Control.Applicative
import Test.QuickCheck
import System.Random
import System.Random.Shuffle
import qualified Data.Map as M
import Data.Tuple(swap)
import Data.Maybe(fromMaybe)
import Control.Monad(foldM)
-- import Debug.Trace

main = doTest 50

doTest ::  Int -> IO ()
doTest maxVertex = do
  let t = genRandomGraph [1..maxVertex]
  ((g,w):_) <- sample' t
  let (_,cost,p) = runDijkstra g 1 maxVertex w
  print (cost,p)
  let pathEdges = zip p (tail p) :: [Edge]
  writeGraphRep g pathEdges 1 maxVertex w "visualization/graph_output.json"

doTest2 ::  Int -> IO ()
doTest2 maxVertex = do
  let t = genRandomGraph [1..maxVertex]
  ((g,w):_) <- sample' t
  print $ search g w 1 maxVertex
  let r@(_,_,p) = runDijkstra g 1 maxVertex w
  print r
  let pathEdges = zip p (tail p) :: [Edge]
  writeGraphRep g pathEdges 1 maxVertex w "visualization/graph_output.json"

-- To generate a random (undirected) Connected Graph you first generate a spanning tree and then add additional edges at random.
--
-- To generate the random tree use the following algorithm
-- dst := random permutation of all nodes;
-- src.push(dst.pop()); % picks the root
-- while (!dst.empty()) {
--     a := random element from src;
--     b := dst.pop();
--     add the edge (a, b)
--     src.push(b);
-- }

genRandomTree ::  [Vertex] -> Gen [Edge]
genRandomTree ns = do
  (d:ds) <- randomPermutation ns
  -- go over all targets and connect each to a random source
  snd <$> foldM makeRandomEdge ([d],[]) ds
    where makeRandomEdge (srcNodes,es) target =
            elements srcNodes >>= \s->return(target:srcNodes,(s,target):es)

genRandomGraph ::  [Vertex] -> Gen (Graph, Weight)
genRandomGraph xs = do
  edgesTree <- genRandomTree xs
  es <- addRandomEdges xs edgesTree
  let symEdges = nub $ foldl (\acc (a,b) -> (a,b):(b,a):acc) [] es
  w <- genSymCostFunction es
  return (buildG (0,length xs) symEdges, w)

genSymCostFunction :: [Edge] -> Gen (Vertex -> Vertex -> Int)
genSymCostFunction es = do
  costs <- foldM (\acc e-> choose (15,70) >>= \c -> return ((e,c):acc)) [] es :: Gen [(Edge,Int)]
  return $ \a b -> let maybeCost = lookup (a,b) costs in
                    fromMaybe (fromMaybe 0 maybeCost) maybeCost

-- prefere vertices that only have one neighbor
addRandomEdges ::  [Vertex] -> [(Int, Int)] -> Gen [(Int, Int)]
addRandomEdges ns es = do
    let upper = truncate $ genericLength ns/3.0 :: Int
    u <- choose (0, upper)
    foldM addEdge es [0..u]
  where addEdge collectedEdges _ = do
          let mp = foldl (\m (a,b)->(M.insertWith (+) b 1 (M.insertWith (+) a 1 m))) M.empty es
          (a,b) <- randomPair (map swap $ M.toList mp)
          return (nub $ (a,b):collectedEdges)

randomPair :: [(Int,Vertex)] -> Gen Edge
randomPair fs = do
  let freqs = [(if count == 1 then 5 else 1,return v) | (count,v) <- fs]
  a <- frequency freqs
  b <- frequency freqs `suchThat` (/= a)
  return (a,b)

randomPermutation ::  [Vertex] -> Gen [Vertex]
randomPermutation xs = arbitrary >>= \x -> return $ shuffle' xs (length xs) (mkStdGen x)

