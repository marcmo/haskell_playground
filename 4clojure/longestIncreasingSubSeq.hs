import Data.List(sortBy,maximumBy,foldl')
import Data.Function(on)

longestSubSeq :: (Eq a, Enum a) => [a] -> [a]
longestSubSeq [] = []
longestSubSeq (x:xs) = if length m == 1 then [] else m
  where (a,b,_) = foldl' buildSeq ([],[x],x) xs
        m = maximumBy (compare `on` length) [a,b] 
        buildSeq (longestSoFar,currentRow,prev) y
          | y == succ prev = (longestSoFar,currentRow++[y],y)
          | otherwise = (maximumBy (compare `on` length) [longestSoFar,currentRow],[y],y)

f = longestSubSeq
test0 = ((==) (f [1,0,1,2,3,0,4,5]) [0,1,2,3])
test1 = ((==) (f [5,6,1,3,2,7]) [5,6])
test2 = ((==) (f [2,3,3,4,5]) [3,4,5])
test3 = ((==) (f [7,6,5,4]) [])
allTests = foldl' (&&) True [test0,test1,test2,test3]
