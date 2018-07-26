module Cluster where

import Control.Arrow ((&&&))
import Data.List(sort)
import qualified Data.Map as M

clusterBy :: Ord b => (a -> b) -> [a] -> [[a]]
clusterBy f = M.elems . M.map reverse . M.fromListWith (++) . map (f &&& return)

groupings f = M.elems . putInMap f

putInMap ::  (Ord k) => (a -> k) -> [a] -> M.Map k [a]
putInMap f = M.map reverse . M.fromListWith (++) . mkPairs f

mkPairs ::  (a -> c) -> [a] -> [(c, [a])]
mkPairs f = map (f &&& return)

-- 	What clusterBy does is group a list of values by their signatures,
-- as computed by a given signature function f, and returns
-- the groups in order of ascending signature.  For example, we
-- can cluster the words “the tan ant gets some fat” by length, by
-- first letter, or by last letter just by changing the
-- signature function we give to clusterBy:


test =  let antwords = words "the tan ant gets some fat"
            res1 = clusterBy length antwords -- [["the","tan","ant","fat"],["gets","some"]]
            res2 = clusterBy head antwords -- [["ant"],["fat"],["gets"],["some"],["the","tan"]]
            res3 = clusterBy last antwords -- [["the","some"],["tan"],["gets"],["ant","fat"]]
            res4 = clusterBy sort antwords in -- [["fat"],["tan","ant"],["gets"],["the"],["some"]]
    [res1,res2,res3,res4]

ff ::  Num a => a -> a
ff x = 2*x
gg ::  Num a => a -> a
gg x = 3*x
foo ::  [a] -> (a, Int)
foo = head &&& length


