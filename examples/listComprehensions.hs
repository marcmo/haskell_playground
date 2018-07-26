
perm xs = [(x,y) | x <- xs, y <- xs]

perm2 xs = (concat (map (\x -> (map (\y -> (x,y)) xs)) xs))

mysum :: Num a => a -> a -> a
--mysum x y = x + y
mysum x = (\y -> x + y)

applyit :: (a -> a) -> a  -> a
applyit f x = f x
