import Test.QuickCheck
import Criterion.Main
import Control.Applicative
import Test.QuickCheck.Gen(Gen(MkGen))
import System.Random

msc :: Ord a => [a] -> Int
msc xs = maximum [scount r rs | r:rs <- tails xs]

scount r rs = length $ filter (>r) rs

tails [] = []
tails xs@(r:rs) = xs:(tails rs)

msc2 ::  (Ord a) => [a] -> Int
msc2 = maximum . (map snd) . table

table ::  (Ord a) => [a] -> [(a, Int)]
table [x] = [(x , 0)]
table xs	= join (m - n) (table ys) (table zs)
            where m =	length xs
                  n = m `div` 2
                  (ys, zs) =	splitAt n xs 

join 0 txs [] = txs
join n [] tys = tys
join n txs@((x,c) : txs') tys@((y,d) : tys')
            | x<y = (x,c+n):join n txs' tys 

            | x>=y = (y,d):join (n-1) txs tys'

----------------------------------------

data TC = TC { values :: [Char] } deriving(Eq,Show)
instance Arbitrary TC where
  arbitrary = TC <$> (listOf1 $ elements ['A'..'Z'])

prop_msc_model (TC xs) = 
  msc xs == msc2 xs

testSamples ::  Int -> IO [[Char]]
testSamples  cnt =
  do rnd0 <- newStdGen
     let (MkGen f) = arbitrary :: Gen TC
     return [values (f r n) | (r,n) <- rnds rnd0 `zip` [0,2..(2*(cnt-1))] ]

rnds ::  (RandomGen t) => t -> [t]
rnds rnd = rnd1 : rnds rnd2 where (rnd1,rnd2) = split rnd

main = do
    testData <- testSamples 20
    defaultMain [
      bgroup "msc" [bench "msc " (whnf msc (head testData))
                   ,bench "msc2" (whnf msc2 (head testData))
                   ]
                 ]

