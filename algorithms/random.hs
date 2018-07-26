import Test.QuickCheck

t :: Int -> (Int,Int) -> Gen [Int]
t n r = vectorOf n (choose r)

randomList n r = head `fmap` sample' (t n r)
  
-- import System.Random
-- import Data.List
-- 
-- main = do
--  seed  <- newStdGen
--  let r = (1,10) :: (Int,Int)
--  let rs = randomlist r 10 seed
--  print rs
-- 
-- -- randomlist :: Int -> StdGen -> [Int]
-- -- randomlist n = take n . unfoldr (Just . random)
-- 
-- randomlist :: Random a => (a,a) -> Int -> StdGen -> [a]
-- randomlist bnds n = take n . randomRs bnds
-- 
-- 
