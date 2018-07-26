import Hubigraph
r x = initHubigraph "http://127.0.0.1:20738/RPC2" >>= runHubigraph x
main = do
  r $ mapM_ newV [1..3]
       where
         newV n = newVertexWithID n >> setVAttr (VCallback url) n
         url = "http://127.0.0.1:19999/callback_serv.bin/vertex_callback"
