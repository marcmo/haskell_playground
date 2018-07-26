import Random
bind :: (a -> StdGen -> (b,StdGen)) -> (StdGen -> (a,StdGen)) -> (StdGen -> (b,StdGen))
bind f x seed = let (x',seed') = x seed in f x' seed'
unit :: Integer -> StdGen -> (Integer, StdGen)
unit x g = (x,g)
lift f = unit . f

addDigit n g = let (a,g') = random g in (n + a `mod` 10,g')
shift = lift (*10)

test :: Integer -> StdGen ->  (Integer,StdGen)
test = bind addDigit . bind shift . addDigit
test2 = (bind shift) . addDigit
test3 = (bind shift)

g = mkStdGen 666
printFun = print $ test2 0 g

main = do 
  print $ test 0 g
  printFun
