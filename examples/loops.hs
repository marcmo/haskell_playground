module Loops where
-- import Debug.Trace
import Test.QuickCheck
import Data.List(foldl')

loop :: Int -> Int -> Int
loop 0 total = total
loop n total = loop (n-1) (total+foo 2)

foo x = 2+x

loop2 :: Int -> Int -> Int
loop2 x y = foldl' (\acc _->acc+foo 2) y [1..x]

prop_loop_model :: Int -> Int -> Property
prop_loop_model x y = x > 0 && y > 0 ==>
  loop x y == loop2 x y

