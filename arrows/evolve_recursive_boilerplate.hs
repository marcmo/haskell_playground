#!/usr/bin/env runhaskell
{-# LANGUAGE TypeSynonymInstances #-}

-- import Data.Random
-- import Data.Random.Source.DevRandom
-- import Data.Random.Extras
import Control.Monad (replicateM)
import Data.Char (ord, chr)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Control.Monad

compete :: [Int] -> IO [Int]
compete pool = return $ 1:[x+1| x <- pool]

evolve :: Int -> [Int] -> IO [Int]
evolve 0 pool = return pool
evolve n pool = compete pool >>= evolve (n - 1)

evolve2 n pool = foldM (const . compete) pool [1..n]
evolve3 n = iterate (compete >=>) return !! n
