import Data.Array
import Control.Monad.ST
import Data.Array.ST
import Data.IORef

buildPair :: (Int, Int)
buildPair = let arr  = listArray (1,10) (repeat 37) :: Array Int Int
                arr' = modifyAsST arr
            in (arr ! 1, arr' ! 1)

modifyAsST :: Array Int Int -> Array Int Int
modifyAsST arr = runST $ do starr <- thaw arr
                            compute starr
                            newarr <- freeze starr
                            return newarr

compute :: STArray s Int Int -> ST s ()
compute arr = do writeArray arr 1 64

main2 = print buildPair

main = do
  let n = 1 :: Int
  x <- newIORef n
  readIORef x >>= print
  writeIORef x 5
  readIORef x >>= print

xs \\ ys = filter (\x->x `notElem` ys) xs
findMin xs = head $ [0..n]\\xs 
  where n = length xs



