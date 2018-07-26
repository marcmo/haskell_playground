import Data.Map
import qualified Data.Map as Map
import qualified Data.Vector as V

bar xs = head xs
foo ::  (Ord k) => Map.Map k a -> k -> Map.Map k a
foo table x = Map.delete x table

primes = unfaithfulSieve [2..]
unfaithfulSieve (p:xs) = p: unfaithfulSieve [x | x <- xs, x `mod` p > 0]

-- sieve xs = sieve' xs Map.empty
--   where sieve' []	table = []
--         sieve' (x:xs) table =
--           case Map.lookup x table of
--           Nothing	−> x : sieve' xs (Map.insert (x*x) [x] table)
--           Just facts	−> sieve' xs (foldl reinsert (Map.delete x table) facts)
--               where
--                 reinsert table prime = Map.insertWith (++) (x+prime) [prime] table

primes2 = sieve2 [2..]
sieve2 xs = sieve' xs Map.empty
  where sieve' [] table = []
        sieve' (x:xs) table =
          case Map.lookup x table of
            Nothing -> x : sieve' xs (Map.insert (x^2) [x] table)
            Just facts -> sieve' xs (foldl reinsert (Map.delete x table) facts)
              where 
                reinsert table prime = Map.insertWith (++) (x+prime) [prime] table

