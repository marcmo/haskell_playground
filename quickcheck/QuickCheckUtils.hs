module QuickCheckUtils

where

import Test.QuickCheck
import Data.List

shuffle :: (Eq a) => [a] -> Gen [a]          
shuffle [] = return []          
shuffle xs = do x  <- oneof $ map return xs
                ys <- shuffle $ delete x xs                          
                return (x:ys)

permutation :: Eq a => [a] -> Gen [a]
permutation initial = inner initial []
  where
    inner :: Eq a => [a] -> [a] -> Gen [a]
    inner [] accum = return accum
    inner xs accum = do
      p <- choose(0,(length xs)-1)
      inner (delete (xs!!p) xs) ((xs!!p):accum)
