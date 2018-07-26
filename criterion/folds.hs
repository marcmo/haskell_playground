import	Criterion.Main
import	Prelude	hiding(foldl)

foldl, foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl f z (x:xs) = foldl f (f z x) xs 
foldl _ z [] = z
foldl' f z (x:xs) = let z' = f z x in
      z' `seq` (foldl' f z' xs)
foldl' _ z [] = z

-- main = defaultMain [ 
--     bench "foldl" $ whnf (foldl (+) 1) [1..10000]
--   , bench "foldl'" $ whnf (foldl' (+) 1) [1..10000]
--   ]
  
doRem :: (Int,Int) -> Int
doRem (x,y) = x `rem` y
doMod :: (Int,Int) -> Int
doMod (x,y) = x `mod` y

main = defaultMain [ 
    bench "rem" $ nf (map doRem) [(x,y) | x <- [1..500], y <- [5,10..10000]]
  , bench "mod" $ nf (map doMod) [(x,y) | x <- [1..500], y <- [5,10..10000]]
  ]

