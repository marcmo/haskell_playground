import Criterion.Main
import Data.List(notElem,partition)
import Data.Array
import Data.Array.ST
import Test.QuickCheck
import Test.QuickCheck.Gen(Gen(MkGen))
import System.Random
import Util.Timer
import Debug.Trace
trc xs = trace (concat xs) -- debug on
-- trc xs a = a -- debug off

minfree ::  (Num a,Enum a) => [a] -> a
minfree xs = head $ [0..] \\ xs

(\\) :: (Eq a) => [a] -> [a] -> [a]
us \\ vs = filter (\x->x `notElem` vs) us

checklist ::  [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0,n) $ zip (filter (<= n) xs) (repeat True)
    where n = length xs

search ::  Array Int Bool -> Int
search = length . (takeWhile id) . elems 

minfree2 :: [Int] -> Int
minfree2 = search . checklist
forceMinfrees2 xs = whnf (map minfree2) xs

countlist :: [Int] -> Array Int Int
countlist xs = accumArray (+) 0 (0,n) (zip xs (repeat 1))
    where n = length xs

sort xs = concat [replicate k x | (x,k) <- assocs $ countlist xs]

checklist2 ::  [Int] -> Array Int Bool
checklist2 xs = runSTArray $ do
                  a <- newArray (0,n) False
                  sequence [writeArray a x True | x <- xs, x <= n]
                  return a
            where n = length xs

minfree3 = search . checklist2

minfree4 :: [Int] -> Int
minfree4 xs = minform 0 (length xs,xs)
minform :: Int -> (Int,[Int]) -> Int
minform a (n,xs) | n == 0 = a
                 | m == b - a = minform b (n-m,vs)
                 | otherwise = minform a (m,us)
                    where (us,vs) = partition (<b) xs
                          b = a + 1 + (n `div` 2) 
                          m = length us

main2 = do
       xs <- testSamples 20
       timeItMsg "minfree" (print $ map minfree xs)
       timeItMsg "minfree2" (print $ map minfree2 xs)
       timeItMsg "minfree3" (print $ map minfree3 xs)
       timeItMsg "minfree4" (print $ map minfree4 xs)
----------------------------------------

data TC = TC { values :: [Int] } deriving(Eq,Show)
instance Arbitrary TC where
  arbitrary = do
    let upper = 10000
    n <- choose(0,upper)
    n2 <- choose(n+2,upper+2)
    return $ TC ([0..n] ++ [n+2..n2])

testSamples ::  Int -> IO [[Int]]
testSamples  cnt =
  do rnd0 <- newStdGen
     let (MkGen m) = arbitrary :: Gen TC
     let xs = [(m r n) | (r,n) <- rnds rnd0 `zip` [0,2..(2*(cnt-1))] ]
     return $ map values xs

rnds ::  (System.Random.RandomGen t) => t -> [t]
rnds rnd = rnd1 : rnds rnd2 where (rnd1,rnd2) = split rnd

prop_minfree_model (TC xs) = res == res2 && res == res3 && res == res4
    where res = minfree xs
          res2 = minfree2 xs
          res3 = minfree3 xs
          res4 = minfree4 xs
    
forceMinfrees xs = whnf (map minfree) xs

main = do
    testData <- testSamples 20
    defaultMain [
      bgroup "minfree" [  -- bench "minfree " (whnf minfree (head testData))
                        bench "minfree2" (whnf minfree2 (head testData))
                        ,bench "minfree3" (whnf minfree3 (head testData))
                        ,bench "minfree4" (whnf minfree4 (head testData))
                       ]
                 ]
