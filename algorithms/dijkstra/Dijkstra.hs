module Dijkstra
  (
    module Data.Graph,
    dijkstra,
    dijkstra2,
    dijkstraS,
    runDijkstra,
    Weight,
    test2,
    testMe
  ) where
import Data.Graph
import Data.Array((!))
import qualified Data.Map as M
import Data.PSQueue
import Control.Monad.State(State,modify,runState)
import Control.Monad(when,foldM)
import Data.Maybe
import Prelude hiding(lookup)

type Weight = Vertex -> Vertex -> Int
type PrevMap = M.Map Vertex Vertex
type PrevState = State PrevMap

-- hinze:
-- The algorithm maintains a queue that maps each vertex to its estimated distance from the source.
-- The algorithm works by
--    * repeatedly removing the vertex with minimal distance and
--    * updating the distances of its adjacent vertices.
-- Priority search queues support both operations equally well.
-- The update operation is typically called decrease.
--
infinity = maxBound :: Int
decrease ::  (Vertex, Int) -> PSQ Vertex Int -> PSQ Vertex Int
decrease (k,p) = adjust (min p) k
decreaseList bs q = Prelude.foldr decrease q bs

dijkstra :: Graph -> Weight -> Vertex -> Vertex -> Maybe (Vertex,Int)
dijkstra g w s t = loop (decrease (s,0) q0)
  where q0 = fromList [v :-> infinity | v <- vertices g]
        loop q1 = maybe Nothing f (minView q1)
          where f (u :-> d,q)
                  | u == t = Just (u,d) -- stop once we found the target
                  | otherwise = loop (decreaseList bs q)
                  where bs = [(v,d + w u v) | v <- neighbors g u]

dijkstraS :: Graph -> Weight -> Vertex -> Vertex -> PrevState (Maybe (Vertex,Int))
dijkstraS g w start end = loop $ decrease (start,0) q0
  where q0 = fromList [v :-> infinity | v <- vertices g]
        loop q1 = maybe (return Nothing) f (minView q1)
          where f (u :-> d,q)
                  | u == end = return $ Just (u,d)
                  | otherwise = decreaseListS u bs q >>= loop
                      where bs = [(v,d + w u v) | v <- neighbors g u]

dijkstra2 :: Graph -> Weight -> Vertex -> [(Vertex,Int)]
dijkstra2 g w s = loop (decrease (s, 0) q0)
  where q0 = fromList [v :-> infinity | v <- vertices g]
        loop q1 = case minView q1 of
          Nothing -> []
          Just(u :-> d, q) -> (u, d ) : loop (decreaseList bs q)
            where bs = [(v, d + w u v ) | v <- neighbors g u]

neighbors g u = g!u


decreaseS :: Vertex-> PSQ Vertex Int -> (Int,Int) -> PrevState (PSQ Vertex Int)
decreaseS u q (k,p) = do
  when (maybe False (p <) (lookup k q)) (updateMap k u)
  return $ adjust (min p) k q

decreaseListS :: Vertex-> [(Vertex, Int)]-> PSQ Vertex Int-> PrevState (PSQ Vertex Int)
decreaseListS u bs q = foldM (decreaseS u) q bs

updateMap :: Int -> Int -> PrevState ()
updateMap k v = modify $ \m -> M.insert k v m

test2 = runDijkstra g1 1 4 meCost

runDijkstra :: Graph -> Vertex -> Vertex -> Weight -> (Vertex, Int, [Vertex])
runDijkstra g src tgt cost = (v,d,traceBack prevMap src tgt)
  where (Just (v,d), prevMap) = runState (dijkstraS g cost src tgt) M.empty

traceBack m start end = walk end []
  where walk n xs
          | n == start = start:xs
          | otherwise = walk (fromJust $ M.lookup n m) (n:xs)

g1 = buildG (1,4) [(1,2),(2,3),(1,3),(2,4),(3,4)]
meCost :: Vertex -> Vertex -> Int
meCost 1 2 = 10
meCost 2 3 = 5
meCost 1 3 = 20
meCost 2 4 = 11
meCost 3 4 = 12
meCost x y = if x == y then 0 else 100
testMe = dijkstra g1 meCost 1 4

