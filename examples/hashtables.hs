import Prelude hiding (lookup)
import Data.HashTable (hashInt, fromList, lookup)

n :: Int
n = 10000000

l :: [Int]
l = [1..n]

stream :: [(Int, Int)]
stream = zip l l

main = do
   m <- fromList hashInt stream
   v <- lookup m 100
   print v

