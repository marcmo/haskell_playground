module MinimumSpanningTree where

import Data.Equivalence.Monad
import Data.Graph as G
import Data.List(sortBy)
import Data.Map as M
import Control.Monad(filterM)
import Data.Ord(comparing)
import Data.Maybe

type Weights = G.Edge -> Int 
fromL :: [(G.Edge,Int)] -> Weights
fromL xs = fromJust . flip M.lookup (M.fromList xs)
run = runEquivM (const ()) (const $ const ())

kruskal :: Ord a => ((Vertex, Vertex) -> a) -> Graph -> [(Vertex, Vertex)]
kruskal weight graph = run $
 filterM go (sortBy (comparing weight) theEdges)
     where
      theEdges = G.edges graph
      go (u,v) = do 
        eq <- equivalent u v
        if eq then return False else
         equate u v >> return True

test = kruskal testWeights testGraph
    where weights = [((1,2),1),((2,3),4),((3,4),5),((1,4),30),((1,3),4)]
          testWeights = fromL weights
          testGraph = G.buildG  (1,4) [e | (e,v) <- weights]

