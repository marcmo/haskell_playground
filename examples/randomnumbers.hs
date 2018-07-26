import System.Random
import Control.Monad
import Data.List
import Control.Monad.State

rand4 :: IO Int
rand4 = getStdRandom (randomR (1, 4))
rand5 :: IO Int
rand5 = getStdRandom (randomR (1, 5))
rand7 = head `liftM` (serie 100)

oneOrTwo = do
  (r,r2) <- two
  if r `mod` 2 == 0 
    then return r2
    else do 
      return $ 4 + r2

serie ::  Int -> IO [Int]
serie n = 
  replicateM n oneOrTwo >>= \s ->
  return $ filter (\x->x/=8) s

two ::  IO (Int, Int)
two = rand4 >>= \r1->
      rand4 >>= \r2->
      return (r1,r2)

verteilungM ::  (Ord a, Monad m) => m [a] -> m [Int]
verteilungM x = verteilung `liftM` x
verteilung :: Ord a => [a] -> [Int]
verteilung = (map length) . group . sort

trueOrFalse :: IO Bool
trueOrFalse = do
  r <- rand4
  return $ (r `mod` 2 == 0)
