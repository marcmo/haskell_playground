import Hubigraph
import Network.XmlRpc.Server
r x = initHubigraph "http://127.0.0.1:20738/RPC2" >>= runHubigraph x
draw :: Int -> IO Int
draw a = do r $ setVAttr (VColor "#ff0000") a >> return 0
main = cgiXmlRpcServer [("vertex_callback", fun draw)]
