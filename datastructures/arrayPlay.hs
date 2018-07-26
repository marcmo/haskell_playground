import Data.Array
import Control.Arrow(first)

testA = let xs = [[1,2,3],[4,5,6],[7,8,9]] in
    sq2ar xs
testB = sq2ar [[1,1,1,5],[2,2,2,6],[3,3,3,7],[4,4,4,8]]

isSym a = a == transp a && ra == transp ra
  where ra = reverseA a

reverseA a = ixmap b (first (x1 -)) a
  where b@(_,(x1,_)) = bounds a

transp a = ixmap (f m,f n) f a
    where f (x,y) = (y,x)
          (m,n) = bounds a

sq2ar ::  [[e]] -> Array (Int, Int) e
sq2ar xs = listArray ((0,0),(dim,dim)) (concat xs)
    where dim = length (head xs) - 1

subSqs :: (Enum b, Num b, Ix b) => Array (b, b) e -> [Array (b, b) e]
subSqs ar = map (\m->ixmap m id ar) bs
    where bs = [((0,0),(b,b))| b <- [1..(x1-1)]] ++ [((a,a),(x1,x1))| a <- [1..(x1-1)]]
          x1 = (fst . snd) (bounds ar)

printA a = do
  let ((x0,y0),(x1,y1)) = bounds a
  let sq = chunks (x1-x0+1) [a!(x,y)| x <- [x0..x1], y <- [y0..y1]]
  mapM_ print sq

chunks ::  Int -> [a] -> [[a]]
chunks n xs = go xs []
  where go [] r = reverse r
        go ys r = let (a,b) = splitAt n ys in go b (a:r)

main = do
  let cs = chunks 5 $ replicate 50 0
  print cs

