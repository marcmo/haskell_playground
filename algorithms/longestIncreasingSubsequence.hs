{-# LANGUAGE BangPatterns #-}
-- By Chris Kuklewicz, copyright 2008, BSD3 license
-- Longest increasing subsequence
-- (see http://en.wikipedia.org/wiki/Longest_increasing_subsequence)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M (empty,null,insert,findMin,findMax
                               ,splitLookup,deleteMin,delete)

type DList a = [a] -> [a]

lnds :: Ord a => [a] -> [a]
lnds = lnds_decode . lnds_fold

lnds_fold :: Ord a => [a] -> Map a (DList a)
lnds_fold = foldl' process M.empty where
  -- The Map keys, in sorted order, are the input values which
  --   terminate the longest increasing chains of length 1,2,3,…
  process mu x =
    case M.splitLookup x mu of
      (_,Just {},_) -> mu -- ignore x when it is already an end of a chain

      (map1,Nothing,map2) | M.null map2 ->
        -- insert new maximum element x
        if M.null mu
          then M.insert x (x:) mu -- x is very first element
          else let !xs = snd (M.findMax mu)
               in M.insert x (xs . (x:)) mu

                          | M.null map1 ->
        -- replace minimum element with smaller x
        M.insert x (x:) (M.deleteMin mu)

                          | otherwise ->
        -- replace previous element oldX with slightly smaller x
        let !xs = snd (M.findMax map1)
            !oldX = fst (M.findMin map2) -- slightly bigger key
            !withoutOldX = M.delete oldX mu
        in M.insert x (xs . (x:)) withoutOldX

lnds_decode :: Ord a => Map a (DList a) -> [a]
lnds_decode mu | M.null mu = []
               | otherwise = snd (M.findMax mu) []

tests =  [ ['b'..'m'] == (lnds ['m'..'s'] ++ ['b'..'g'] ++ ['a'..'c'] ++ ['h'..'k'] ++ ['h'..'m'] ++ ['d','c'..'a'])
         , "" == lnds ""
         , "a" == lnds "a"
         , "a" == lnds "ba"
         , "ab" == lnds "ab"
         ]
