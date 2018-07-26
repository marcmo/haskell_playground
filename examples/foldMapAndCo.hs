foo x = [x,x*x]

f xs = concat (map foo xs)

getArea:: Eq a => a -> [[a]] -> Bool
getArea _ [] = False
getArea x areas = foldl (\x y -> x && y) False map elem a

