import Data.List
import qualified Data.List.Key as K
import Data.Map ((!), fromList, fromListWith, adjust, keys, Map)

-- In order make the rest of the algorithm simpler, we convert the list of edges to a map that lists
-- all the neighbors of each vertex.

buildGraph :: Ord a => [(a, a, Float)] -> Map a [(a, Float)]
buildGraph g = fromListWith (++) $ g >>=
               \(a,b,d) -> [(a,[(b,d)]), (b,[(a,d)])]

-- The algorithm follows the usual steps, albeit in a functional rather than the typical procedural style:
-- start by giving all non-source vertices an infinite distance, then go through all the vertices in order
-- of their distance from the source, relaxing all their neighbors.

dijkstra :: Ord a => a -> Map a [(a, Float)] -> Map a (Float, Maybe a)
dijkstra source graph =
    f (fromList [(v, (if v == source then 0 else 1/0, Nothing))
                | v <- keys graph]) (keys graph) where
    f ds [] = ds
    f ds q  = f (foldr relax ds $ graph ! m) (delete m q) where
              m = K.minimum (fst . (ds !)) q
              relax (e,d) = adjust (min (fst (ds ! m) + d, Just m)) e

-- Getting the shortest path is then simply a matter of tracing the path from the endpoint to the beginning.

shortestPath :: Ord a => a -> a -> Map a [(a, Float)] -> [a]
shortestPath from to graph = reverse $ f to where
    f x = x : maybe [] f (snd $ dijkstra from graph ! x)

-- A test to see if everything is working properly:

main :: IO ()
main = do let g = buildGraph [('a','c',2), ('a','d',6), ('b','a',3)
                             ,('b','d',8), ('c','d',7), ('c','e',5)
                             ,('d','e',10)]
          print $ shortestPath 'a' 'e' g == "ace"
