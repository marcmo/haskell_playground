-- import Control.Monad.Par
import System.Environment

collatz :: Integer -> Integer
collatz n
  | even n = n `div` 2
  | odd n  = 3 * n + 1

collatzSeq :: Integer -> [Integer]
collatzSeq = takeWhile (> 1) . iterate collatz

collatzMax :: Integer -> Integer -> Int
collatzMax lo hi = maximum (map (length . collatzSeq) [lo .. hi])

main :: IO ()
main = do
  [n] <- getArgs
  print $ collatzMax 1 (read n)
