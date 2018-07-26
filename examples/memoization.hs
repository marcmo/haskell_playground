import qualified Data.MemoTrie as A
import qualified Data.MemoCombinators as B
import Data.Bits
import Debug.Trace

-- naive
fib  :: Integer -> Integer
fib  1 = 1
fib  2 = 1
fib n = fib (n - 1) + fib (n - 2)

-- better
memoized_fib :: Int -> Integer
memoized_fib =
   let fib 0 = 0
       fib 1 = 1
       fib n = memoized_fib (n-2) + memoized_fib (n-1)
   in  (map fib [0 ..] !!)

-- unbelievable
fibA :: Integer -> Integer
fibA = A.memo fibA' where
  fibA' 1 = 1
  fibA' 2 = 1
  fibA' n = fibA (n - 1) + fibA (n - 2)
  
fibB :: Integer -> Integer
fibB = B.integral fibB' where
  fibB' 1 = 1
  fibB' 2 = 1
  fibB' n = fibB (n - 1) + fibB (n - 2)

fibCountingB :: Int -> Integer -> (Int,Integer)
-- fibCountingB = B.memo2 B.integral B.integral fibCountingB' where
fibCountingB = B.memoSecond (B.integral) fibCountingB' where
  fibCountingB' m 1 = (m,1)
  fibCountingB' m 2 = (m,1)
  fibCountingB' m n = (trace $ "fibCountinbB for " ++ show (m,n))(n1 + n2, x1 + x2)
    where (n1,x1) = fibCountingB (m+1) (n - 1)
          (n2,x2) = fibCountingB (m+2) (n - 2)

sillyFib :: String -> String
sillyFib "1" = "1"
sillyFib "2" = "1"
sillyFib xs = let x = read xs in
  (trace $ "sillyFib for " ++ xs)show $ (read $ sillyFib $ show (x-1)) + (read $ sillyFib $ show (x-2))

sillyFibB :: String -> String
sillyFibB = (B.list B.char) sillyFibB'
  where sillyFibB' "1" = "1"
        sillyFibB' "2" = "1"
        sillyFibB' xs = let x = read xs in
          (trace $ "sillyFibB for " ++ xs) show $ (read $ sillyFibB $ show (x-1)) + (read $ sillyFibB $ show (x-2))

--complex setup
-- fac :: Int -> Integer
fac 0 = 0
fac 1 = 1
fac n = n*fac(n-1)

-- take the number of nodes, the edge cost function and
-- return the tour with minimal cost (ATSP).
solveTSP :: Int -> (Int -> Int -> Int) -> Int
solveTSP n cost = solver (n-1) (1 `shiftL` (n-1) - 1)
  where solver = B.memo2 (range n) (range $ 2^(n-1)) puresolver
        range n = B.unsafeArrayRange (0::Int,n-1)
        puresolver :: Int -> Int -> Int
        puresolver y 0 = cost y (n-1)
        puresolver y set = minimum $ map (\x -> (cost y x) + solver x (set `clearBit` x)) (filter (set `testBit`) [0..(n-1)]) 

cost :: Int -> Int -> Int
cost m n = 5

expensive :: String -> Int
expensive xs = (trace $ "called with " ++ xs)length xs

expensiveUsingMemo :: String -> Int
expensiveUsingMemo = (B.list B.char) f
  where f = expensive

