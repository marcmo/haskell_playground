import System.Random
import Test.QuickCheck
import Data.List
import Criterion.Main
import Control.Applicative
import qualified Data.Vector.Unboxed as V

tio ::  Int -> IO (V.Vector Int)
tio n = V.fromList <$> head <$> sample' (vectorOf n $ choose (1,100000000000000))

genVector :: IO (V.Vector Int)
genVector = do
  print "start genVector"
  seed  <- newStdGen
  let rs = randomlist 3000000 seed
  let vec = V.fromList rs
  print "stop genVector"
  return vec

sumit :: V.Vector Int -> IO Int
sumit vec = do
  return $ V.sum vec
 
randomlist :: Int -> StdGen -> [Int]
randomlist n = take n . unfoldr (Just . random)

main = do 
  let vv = tio 3000000
  whnfIO vv
  v <- vv
  defaultMain [bench "sumit" $ whnfIO $ sumit v]
