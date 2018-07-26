module DynamicProgramming where
-- Given a list of N coins,
-- their values (V1, V2, ... , VN),
-- and the total sum S.
--
-- Find the minimum number of coins the sum of which is S (we can use as many coins of one type as we want), or report that it's not possible to select coins in such a way that they sum up to S.

import qualified Data.Vector as V
import Data.List(foldl')
import Control.Applicative
import Control.Monad(mplus,guard)
import Data.Array
import Data.Bits
import Test.QuickCheck(Arbitrary,arbitrary,choose)

type Coin = Int

interleave :: a -> [a] -> [[a]]
interleave x [] =  [[x]]
interleave x (y:ys) =  (x:y:ys) : map (y:) (interleave x ys)

backtrack :: [a] -> [[a]]
backtrack [] = [[]]
backtrack (x:xs)=  concat (map (interleave x) (backtrack xs))

pow [] = [[]]
pow (i:is) = [ j ++ l | l <- pow is, j <- [[], [i]] ]

findsum :: [Integer] -> Integer -> [[Integer]]
findsum ls k = filter f (pow ls) where f xs = sum xs == k

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = rest ++ map (x:)rest
  where rest = subsets xs

powerset ::  [b] -> [[b]]
powerset = foldl' (\acc x -> acc ++ map (x:) acc) [[]]

-- rather inefficient
powerset2 ::  [a] -> V.Vector (V.Vector a)
powerset2 xs = V.foldl' (\acc x -> acc V.++ V.map (V.cons x) acc) (V.fromList [V.empty]) (V.fromList xs)

-- sumValue :: Int -> [Coin] -> Seq Coin
-- sumValue target coins = loop S.empty
  -- where loop s = foldl' (\acc x-> acc) S.empty [0..length coins]

-- Set Min[i] equal to Infinity for all of i
-- Min[0]=0
--
-- For i = 1 to S
-- For j = 0 to N - 1
--    If (Vj<=i AND Min[i-Vj]+1<Min[i])
--     Then Min[i]=Min[i-Vj]+1
--
-- Output Min[S]


buyable n = r!n
    where r = listArray (0,n) (True : map f [1..n])
          f i = i >= 6 && r!(i-6) || i >= 9 && r!(i-9) || i >= 20 && r!(i-20)
-- using vector:
buyable2 n = r V.!n
    where r = V.cons True (V.tail $ V.generate (n+1) f)
          f i = i >= 6 && r V.!(i-6) || i >= 9 && r V.!(i-9) || i >= 20 && r V.!(i-20)
newtype MyN = MyN Int deriving (Show,Eq)
instance Arbitrary MyN where
  arbitrary = MyN <$> choose (0,10000)

prop_buyable1_2 :: MyN -> Bool
prop_buyable1_2 (MyN n) =
  buyable n == buyable2 n

buy n = r!n
    where r = listArray (0,n) (Just (0,0,0) : map f [1..n])
          f i = case attempt (i-6)
                of Just(x,y,z) -> Just(x+1,y,z)
                   _ -> case attempt (i-9)
                        of Just(x,y,z) -> Just(x,y+1,z)
                           _ -> case attempt (i-20)
                                of Just(x,y,z) -> Just(x,y,z+1)
                                   _ -> Nothing
          attempt x = if x>=0 then r!x else Nothing
buy2 n = r!n
    where r = listArray (0,n) (Just (0,0,0) : map f [1..n])
          -- f i = attempt (i-6) >>= \(x,y,z) -> Just (x+1,y,z)
          f i = do (x,y,z) <- attempt (i-6)
                   return (x+1,y,z)
                `mplus`
                do (x,y,z) <- attempt (i-9)
                   return (x,y+1,z)
                `mplus`
                do (x,y,z) <- attempt (i-20)
                   return (x,y,z+1)
          attempt x = guard (x>=0) >> r!x

prop_buy1_2 :: MyN -> Bool
prop_buy1_2 (MyN n) =
  buy n == buy2 n

buyable3 m = iter m (True : replicate 19 False)
    where iter 0 lst = lst !! 0
          iter n lst = iter (n-1) ((lst !! 5 || lst !! 8 || lst !! 19) : take 19 lst)

prop_buyable1_3 :: MyN -> Bool
prop_buyable1_3 (MyN n) =
  buyable n == buyable3 n
 
buyable4 m = iter m 1
    where iter :: Int -> Int -> Bool
          iter 0 lst = odd lst
          iter n lst = iter (n-1) ((lst `shiftL` 1) .|.
                                   if lst .&. 0x8120 /= 0 then 1 else 0)

