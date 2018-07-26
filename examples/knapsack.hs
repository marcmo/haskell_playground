module Knapsack where
import Control.Monad(filterM,replicateM)
import Data.Array
import Data.List
import qualified Data.Vector as V
import Test.QuickCheck
import Debug.Trace

data Item a = Item { item :: a,
  itemValue :: Int,
  itemSize :: Int } deriving (Eq, Ord)
instance (Show a) => Show (Item a) where
  show (Item i v s) = show i ++ ",(v=" ++ show v ++ ",s:" ++ show s ++ ")"

data Cell a = Cell (Int, [Item a]) deriving (Eq, Show, Ord)

powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

brutepack :: (Ord a) => Int -> [Item a] -> Cell a
brutepack size items = maximum [
  cellOf subset | subset <- powerset items, itemsFit subset]
    where
    itemsFit is = sum (map itemSize is) <= size
    cellOf is = Cell (sum (map itemValue is), is)

type PackSize = Int
type ItemList a = V.Vector (Item a)
data Zelle a = Zelle Int [Item a] deriving (Eq,Show,Ord)
daypack :: PackSize -> ItemList a -> Zelle a
daypack size items = valOf noItems size
  where noItems = V.length items
        table = V.fromList[V.fromList[(trace $ "calculate cell" ++ show m ++ "," ++ show n)(cell m n)| n<-[1..size]]| m <- [1..noItems]]
        valOf m n
          | m < 1 || n < 1 = Zelle 0 []
          | otherwise = (table V.! (m-1)) V.! (n-1)
        cell m n = case items V.! (m-1) of
          i@(Item _ v s)
            | s > n || vL >= vU + v -> Zelle vL iL
            | otherwise             -> Zelle (vL + v) (i:iL)
            where Zelle vL iL = valOf (m-1) n
                  Zelle vU _ = valOf (m-1) (n-s)

examleItems = [Item "Lion" 30 4, Item "Tiger" 20 3,Item "Bear" 40 5]
dynapack :: Int -> [Item a] -> Cell a
dynapack size items = valOf noItems size
  where noItems = length items
        itemsArr = listArray (1,noItems) items
        itemNo n = itemsArr ! n

        table = array ((1,1),(noItems,size))
          [ ((m, n), cell m n) | m <- [1..noItems],
                                 n <- [1..size] ]

        valOf m n
          | m < 1 || n < 1 = Cell (0, [])
          | otherwise = table ! (m,n)

        cell m n = case itemNo m of
          i@(Item _ v s)
            | s > n || vL >= vU + v -> Cell (vL , isL)
            | otherwise             -> Cell (vU + v, i:isU)
            where Cell (vL, isL) = valOf (m - 1) n
                  Cell (vU, isU) = valOf (m - 1) (n - s)

newtype TestItems = TestItems [(Int, Int, Int)] deriving (Eq, Show, Ord)

nubWithKey k = nubBy (\a b -> k a == k b)

fst3 (a,_,_) = a

tripleToItem (i,v,s) = Item i v s

instance Arbitrary TestItems where
  arbitrary = sized $ \n -> do
    items <- replicateM n $ do
                  i <- arbitrary
                  v <- choose (1, n)
                  s <- choose (1, n)
                  return (i, v, s)
    return . TestItems . nubWithKey fst3 $ items

prop_effectivePacking :: TestItems -> Bool
prop_effectivePacking (TestItems items) = traceShow items $ v1 == v2
  where items' = map tripleToItem items
        Cell (v1,_) = brutepack 10 items'
        Cell (v2,_) = dynapack 10 items'








