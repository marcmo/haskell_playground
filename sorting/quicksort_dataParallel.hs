{-# LANGUAGE PArr, ParallelListComp #-}
{-# OPTIONS -fvectorise #-}

import qualified Prelude
import Data.Array.Parallel.Prelude
import Data.Array.Parallel.Prelude.Double

qsort :: Ord a = > [: a :] -> [: a :]
qsort [::] = [::]
qsort xs = let
p = xs !: ( lengthP xs ’ div ’ 2) -- pivot aus Mitte
ss = [: s | s <- xs , s < m :]
ms = [: s | s <- xs , s == m :]
gs = [: s | s <- xs , s > m :]
sorted = [: qsort xs ’ | xs ’ <- [: ss , gs :]:]
in
( sorted !: 0) +:+ ms +:+ ( sorted !:1)

