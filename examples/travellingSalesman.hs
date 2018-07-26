import Data.List
import Data.Bits
import qualified Data.MemoCombinators as B
import qualified Data.List.Key as K

dist :: Floating a => (a, a) -> (a, a) -> a
dist (x1, y1) (x2, y2) = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)

tours :: [(Int,b)] -> [[(Int, b)]]
tours = map (\(x:xs) -> x:xs ++ [x]) . permutations

cost :: Floating a => [(b, (a, a))] -> a
cost xs = sum $ zipWith dist xss (tail xss)
  where xss = map snd xs

shortestPath :: [(Double, Double)] -> [Int]
shortestPath = init . map fst . K.minimum cost . tours . zip [0..]

main = do
  let sp = shortestPath [(5, 2), (19, 13), (4, 8), (6, 32), (23, 7), (57, 54), (55, 8), (70, 59)]
  print $ "path:" ++ show sp


exampleCost :: Int -> Int -> Int
exampleCost 0 1 = 3
exampleCost 1 2 = 4
exampleCost 1 3 = 6
exampleCost 3 4 = 4
exampleCost 2 4 = 9
exampleCost _ _ = 1000

-- take the number of nodes, the edge cost function and
-- return the tour with minimal cost (ATSP).
-- type CostFun a = Floating a => (a, a) -> (a, a) -> a
type CostFun a = (a, a) -> (a, a) -> a
-- solveTSP :: Int -> (Int -> Int -> Int) -> Int
solveTSP :: Int -> (Int -> Int -> Int) -> Int
solveTSP n edgeCost = solver (n-1) (1 `shiftL` (n-1) - 1)
  where solver = B.memo2 (range n) (range $ 2^(n-1)) puresolver
        range n = B.unsafeArrayRange (0::Int,n-1)
        puresolver :: Int -> Int -> Int
        puresolver y 0 = edgeCost y (n-1)
        puresolver y set = minimum $ map (\x -> (edgeCost y x) + solver x (set `clearBit` x)) (filter (set `testBit`) [0..(n-1)])


-- brute_force cities =
-- 	    other_cities = cities.copy()
-- 	    start_city = tuple(other_cities.pop())
-- 	    return min((start_city+p for p in permutations(other_cities)), key=path_cost)





