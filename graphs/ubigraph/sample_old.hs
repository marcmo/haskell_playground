import Hubigraph
import qualified Hubigraph.Old as O

r x = initHubigraph "http://localhost:20738/RPC2" >>= runHubigraph x

main = r $ mkRing 10

mkRing n = do mapM_ (newVertexWithID) all
              mapM_ (newEdge') all
              mapM_ (\vid -> O.vertexColor vid red) all
              mapM_ (\vid -> O.vertexShape vid O.Sphere) all
    where newEdge' e = newEdge (e, next e)
          next e = (e+1) `mod` n
          all = [0..last]
          last = n-1
          red = "#ff0000"
