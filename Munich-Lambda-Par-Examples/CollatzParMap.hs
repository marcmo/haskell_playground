import Control.Monad.Par
import System.Environment
import Data.List.Split

collatz :: Integer -> Integer
collatz n
  | even n = n `div` 2
  | odd n  = 3 * n + 1

collatzSeq :: Integer -> [Integer]
collatzSeq = takeWhile (> 1) . iterate collatz

parCollatzMax :: Int -> Integer -> Integer -> Int
parCollatzMax chunkSize lo hi =
  maximum $ concat $ runPar (parMap (map (length . collatzSeq)) chunks)
    where chunks = chunksOf chunkSize [lo..hi]

main :: IO ()
main = do
  [n] <- getArgs
  print $ parCollatzMax 10000 1 (read n)
