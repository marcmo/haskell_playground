module Main
  where
import Control.Monad
import Data.List
import Test.QuickCheck
import Debug.Trace
type Pos = Int
data Sudoku = Sudoku [Char] deriving (Show)

prop_noRepeats :: Sudoku -> Bool
prop_noRepeats s@(Sudoku xs) = 
  trace (show s) $ all ((==1) . length) $
                   filter ((/='.') . head) $                   
                   group $ sort xs

instance Arbitrary Sudoku
  where  arbitrary = sized board :: Gen Sudoku   
            where board :: Int -> Gen Sudoku         
                  board 0 = Sudoku `liftM` shuffle values         
                  board n | n > 6 = resize 6 arbitrary                 
                          | otherwise =                     
                              do xs <- shuffle values                         
                                 let removed = take n xs                             
                                     dots = take n $ repeat '.'                             
                                     remain = values \\ removed                         
                                 ys <- shuffle $ dots ++ remain                         
                                 return $ Sudoku ys         
                  values = ['1' .. '9']
                  shuffle :: (Eq a) => [a] -> Gen [a]         
                  shuffle [] = return []         
                  shuffle xs = do
                      x  <- oneof $ map return xs                         
                      ys <- shuffle $ delete x xs                         
                      return (x:ys)
