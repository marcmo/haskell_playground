-- CatA:
-- now = 3102,66
-- next = 1.9.2014
-- dann 3352,66 (ab 1.10.2009)
-- dann(1.9.2019) 3502,66
-- 
-- CatB:
-- now = 2946,43
-- ab 1.9.2011 3211,40
-- nach 5 jahren 3423,37
import Data.List
import Monad

getTotal :: [Int] -> Int ->  Int
getTotal xs month =
    foldl' (\acc m->acc+m) 0 (take month xs)

moneyPerMonthA = replicate (5*12) 3103 ++ replicate (5*12) 3353 ++ repeat 3503
moneyPerMonthB = replicate (2*12) 2946 ++ replicate (2*12) 3211 ++ repeat 3423

findBreakEven n = do
  let moneyA = map (getTotal moneyPerMonthA) [0..]
  let moneyB = map (getTotal moneyPerMonthB) [0..]
  let comp = take n $ zip moneyA moneyB
  mapM_ printDiff (zip [1..] comp)

printDiff (n,(x,y)) = do
  if x > y then putStrLn $ show n ++ ".) (" ++ show (x-y) ++ ") >>>>>>> " ++ show x ++ " --- " ++ show y
    else putStrLn $ show n ++ ".) (" ++ show (x-y) ++ ") " ++ show x ++ " --- " ++ show y ++ " <<<<<<<<"

