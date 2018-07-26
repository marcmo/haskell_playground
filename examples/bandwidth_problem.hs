import Data.List
import Maybe

type Vertex = Int
data Edge = Edge Vertex Vertex deriving (Eq,Show)
type Graph = [Edge]

bandwidth :: Graph -> [Vertex]
bandwidth g = []

nodes [0,1,2,3]
example = [Edge 0 1, Edge 1 2, Edge 2 3, Edge 0 3]

largest es = foldl' (\acc (Edge a b) -> min acc (dist a b es)) 0 es

dist :: Vertex -> Vertex -> [Vertex] -> Graph -> Int
dist a b vs g = let i = fromJust $ elemIndex a vs
                    j = fromJust $ elemIndex b vs in



