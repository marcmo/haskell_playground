import qualified Data.Vector.Unboxed as U
import System.Random.Mersenne
import qualified Data.Vector.Random.Mersenne as G

main = do
    g <- newMTGen Nothing
    a <- G.random g 10000000 :: IO (U.Vector Double) -- 100 M
    print (U.sum a)
