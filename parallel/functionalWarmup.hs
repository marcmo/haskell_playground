
conc :: [[a]] -> [a]
conc xs = foldl (\acc x->acc ++ x) [] xs




