import System
fac:: (Num a, Enum a) => a -> a
fac = foldr (*) 1 . enumFromTo 1

main = do 
  [n] <- getArgs
  print  $ show $ fac (read n)
    
