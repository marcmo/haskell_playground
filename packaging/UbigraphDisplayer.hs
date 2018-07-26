module UbigraphDisplayer

where

import Hubigraph
import Control.Monad.Reader (liftIO)
import System.Posix

r x = initHubigraph "http://127.0.0.1:20738/RPC2" >>= runHubigraph x

main2 ::  IO ()
main2 = do
  r $ clear
  r $ sample 100
  
sample ::  VertexID -> Hubigraph ()
sample n = do newNode "start" 0
              mapM_ build [1..n]
  where build x = do
              newNode "t" x 
              newNode "v" (x + n)
              newDep x (x-1)
              newDep x (x+n)
              delay (50000-100*x)

newDep ::  Int -> Int -> Hubigraph ()
newDep a b = do
  let edgeId = mkId a b
  newEdgeWithID edgeId (a,b)
  edgeStyle <- arrowStyle
  changeEStyle edgeStyle edgeId
  return ()

newNode :: String -> VertexID -> Hubigraph ()
newNode n id = do
  newVertexWithID id
  style <- nodeStyle n
  changeVStyle style id
  return ()

delay n = r $ (liftIO ((usleep n) >> return ()))

mkId :: Int -> Int -> Int
mkId a b = 10*a+b

arrowStyle ::  Hubigraph StyleID
arrowStyle = do sid <- newEStyle 0
                applyEdgeStyle [EOriented True,EArrow True,EStrength 0.1] sid 
                return sid
applyEdgeStyle as sid = mapM_ (\a-> setEStyleAttr a sid) as

nodeStyle :: String -> Hubigraph StyleID
nodeStyle n = do sid <- newVStyle 0
                 setVStyleAttr' [VSize 0.5,VColor red, VShape Sphere, VLabel n] sid
                 return sid
    where setVStyleAttr' vas sid = mapM_ (\va -> setVStyleAttr va sid) vas
          red = "#ff0000"

