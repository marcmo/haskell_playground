import Control.Monad.Writer  
import qualified Data.Map as M
  
logNumber :: Int -> Writer [String] Int  
logNumber x = writer (x, ["Got number: " ++ show x])  

multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    return (a*b)  

type IntMap = M.Map Int Int

logNumber2 :: Int -> Writer IntMap Int  
logNumber2 x = writer (x, M.fromList [(x,x)])  
  
multWithLog2 :: Writer IntMap Int  
multWithLog2 = do  
    a <- logNumber2 3  
    b <- logNumber2 5  
    return (a*b)  
