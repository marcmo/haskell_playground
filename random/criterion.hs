import Criterion.Main
import Data.Vector as V

vector :: IO (Vector Int)
vector = do
  let vec = V.replicate 3000000 10
  return vec

sumit :: IO Int
sumit = do
  vec <- vector
  return $ V.sum vec

main = defaultMain [bench "sumit" $ whnfIO sumit]

