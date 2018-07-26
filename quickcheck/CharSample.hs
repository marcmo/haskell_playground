
import Data.Char
import Test.QuickCheck
import Test.QuickCheck.Test
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
 
import Data.List

newtype MyChar = MC { unC :: Char } deriving Show
instance Arbitrary MyChar where
    arbitrary     = do
      c <- choose ('\32', '\128')
      return $ MC c


deepCheck = quickCheckWith stdArgs { maxSuccess = 1000, maxSize = 1000 }

main = defaultMain tests

tests = [
          testProperty "take5" prop_take5,
          testProperty "sort2" prop_sort1
        ]


-- A thin monadic skin layer
getList :: IO String
getList = fmap take5 getContents

-- The actual worker
take5 :: String -> String
take5 = take 5 . filter (`elem` ['a'..'e'])

prop_take5 xs = length xs > 100 ==> length (take5 (map unC xs)) == 5
  where types = xs :: [MyChar]

prop_sort1 xs = sort xs == sortBy compare xs
  where types = xs :: [Int]

