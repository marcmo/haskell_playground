import Control.Monad.Par
import System.Environment

collatz :: Integer -> Integer
collatz n
  | even n = n `div` 2
  | odd n  = 3 * n + 1

collatzSeq :: Integer -> [Integer]
collatzSeq = takeWhile (> 1) . iterate collatz

collatzMax :: Integer -> Integer -> Int
collatzMax lo hi = maximum (map (length . collatzSeq) [lo .. hi])

parCollatzMax :: Integer -> Integer -> Int
parCollatzMax lo hi = runPar $ do
  r1 <- spawnP (collatzMax lo mi)
  r2 <- spawnP (collatzMax (mi + 1) hi)
  m1 <- get r1
  m2 <- get r2
  return (max m1 m2)
    where
      mi = (lo + hi) `div` 2

main :: IO ()
main = do
  [n] <- getArgs
  print $ parCollatzMax 1 (read n)
