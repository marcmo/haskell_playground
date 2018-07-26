import Hubigraph
import Control.Monad.Reader (liftIO)
import System.Posix

r x = initHubigraph "http://127.0.0.1:20738/RPC2" >>= runHubigraph x

main ::  IO ()
main = do
  r $ clear
  -- let a = 
  -- r $ connectAll [(1,2),(2,3),(3,1),(3,4),(4,5),(5,1),(1,6),(1,7),(1,4)]
  r $ sample 100
  -- r $ newNode "eeprom" 1
  -- r $ newNode "nvram" 2
  -- r $ newDep 1 2
  -- delay 1
  -- r $ newNode "flash" 3
  -- r $ newDep 1 3
  -- delay 1
  -- r $ newNode "common" 4
  -- r $ newDep 1 4
  -- r $ newDep 2 4
  -- delay 1
  -- r $ newNode "other" 5
  -- r $ newDep 5 1
  -- delay 1
  -- delay 1
  
sample n = do newNode "start" 0
              mapM_ build [1..n]
  where build x = do
              newNode "t" x 
              newNode "v" (x + n)
              newDep x (x-1)
              newDep x (x+n)
              delay (50000-100*x)

newDep a b = do
  let edgeId = mkId a b
  newEdgeWithID edgeId (a,b)
  edgeStyle <- arrowStyle
  changeEStyle edgeStyle edgeId
  return ()

newNode n id = do
  newVertexWithID id
  style <- redStyle n
  changeVStyle style id
  return ()
  

connectAll :: [(VertexID, VertexID)]-> Hubigraph ()
connectAll = mapM_ (\(a,b)-> do connect a b
                                delay 1)
delay n = r $ (liftIO ((usleep n) >> return ()))

connect a b = do
  va <- newVertexWithID a
  vb <- newVertexWithID b
  let edgeId = mkId a b
  newEdgeWithID edgeId (a,b)
  edgeStyle <- arrowStyle
  changeEStyle edgeStyle edgeId
  style <- redStyle "hi"
  mapM_ (changeVStyle style) [a,b]

mkId :: Int -> Int -> Int
mkId a b = 10*a+b

arrowStyle ::  Hubigraph StyleID
arrowStyle = do sid <- newEStyle 0
                applyEdgeStyle [EStroke Dotted,EArrow True] sid 
                return sid
applyEdgeStyle as sid = mapM_ (\a-> setEStyleAttr a sid) as

redStyle :: String -> Hubigraph StyleID
redStyle n = do sid <- newVStyle 0
                setVStyleAttr' [VColor red, VShape Cube, VLabel n] sid
                return sid
    where setVStyleAttr' vas sid = mapM_ (\va -> setVStyleAttr va sid) vas
          red = "#ff0000"

mkRing :: Int -> VertexID -> Hubigraph ()
mkRing x n = do mapM_ (newVertexWithID) all
                mapM_ (newEdge') all
                sid <- newVStyle 0
                setVStyleAttr' [VColor red, VShape Sphere] sid
                mapM_ (changeVStyle sid) all
    where newEdge' e = newEdge (e, next e)
          next e = (e+1) `mod` n
          all = [x..last]
          last = n-1
          red = "#ff0000"
          setVStyleAttr' vas sid = mapM_ (\va -> setVStyleAttr va sid) vas

