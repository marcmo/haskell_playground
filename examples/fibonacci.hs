import Data.List
import Data.Bits
import System

fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib2 :: Int -> Integer
fib2 n = snd . foldl' fib' (1, 0) . dropWhile not $
            [testBit n k | k <- let s = bitSize n in [s-1,s-2..0]]
    where
        fib' (f, g) p
            | p         = (f*(f+2*g), ss)
            | otherwise = (ss, g*(2*f-g))
            where ss = f*f+g*g

fib3 ::  Int -> Integer
fib3 n = fibs!!n

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main = do
  x:[] <- getArgs
  -- print $ fib2 $ read x
  print $ length $ show $ fib2 $ read x

