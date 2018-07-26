module Pathfinder(search)
where

import Data.Graph
import Data.Array((!))
import Data.PSQueue hiding(foldl)
import qualified Data.Set as S

type Weight = Vertex -> Vertex -> Int

search :: Graph -> Weight -> Vertex -> Vertex -> Maybe (Vertex,Int)
search g w s e = loop S.empty (s:->0, empty) 
  where loop visited (x:->d,open)
          | x == e = return (x,d)
          | otherwise = maybe Nothing (loop (S.insert x visited)) sorted
            where sorted = minView $ foldl updateMin open nDists
                  nDists = [(n,d + w x n)| n <- neighbors, n `S.notMember` visited]
                  neighbors = g!x 
                  updateMin q (n,dis) = alter (maybe (Just dis) (Just . min dis)) n q

-- dijkstra :: Graph -> Weight -> Vertex -> Vertex -> Maybe (Vertex,Int)
-- dijkstra g w s t = loop (decrease (s,0) q0)
--   where q0 = fromList [v :-> infinity | v <- vertices g]
--         loop q1 = maybe Nothing f (minView q1)
--           where f (u :-> d,q) 
--                   | u == t = Just (u,d) -- stop once we found the target
--                   | otherwise = loop (decreaseList bs q) 
--                   where bs = [(v,d + w u v) | v <- neighbors g u]
--
-- idea: list of nodes we want to investigate further: OPEN
-- start -> OPEN
-- pop of OPEN, find neighbors, note gValue (how many expansion we did so far)
-- pick node from OPEN with smalles gValue
--

