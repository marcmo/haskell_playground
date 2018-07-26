import Data.List (foldl')
import qualified Data.MemoCombinators as B

seqSize = 100
numShifts = 10000 

cycleShift s l = drop (fromInteger s) l ++ take (fromInteger s) l

modAdd s t = zipWith comb s t
comb ::  Integral a => a -> a -> a
comb a b = foo (a + b)
  where foo n = B.integral foo' n
          where foo' x = x `rem` 10^16

comb2 ::  Integer -> Integer -> Integer
comb2 = (B.integral . B.integral) comb' where
  comb' ::  Integer -> Integer -> Integer
  comb' a b = (a + b) `rem` 10^16

comb3 ::  Integer -> Integer -> Integer
comb3 = B.integral comb'
  where comb' a b = (a + b) `rem` 10^16

step l shift = modAdd l (cycleShift shift l)

allshifts = [i `rem` seqSize |i <- [1..numShifts]]
start = 1 : [0 | i <- [1..(seqSize - 1)]]
end = foldl' step start allshifts

main :: IO ()
main = print (end !! 0)

