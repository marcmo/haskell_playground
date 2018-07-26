import Data.List

perm ::  (Eq a) => [a] -> [[a]]
perm [] = [[]]
perm str = [x:xs | x <- str, xs <- perm (delete x str)]

perms ::  [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = [ p ++ [x] ++ s | xs' <- perms xs,
                                 (p, s) <- zip (inits xs') (tails xs')]

--more efficient
permutations [] = [[]]
permutation xs = [y : ps | (y,ys) <- selections xs, ps <- permutation ys]
selections []     = []
selections (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- selections xs]




