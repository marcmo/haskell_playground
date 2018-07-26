import Test.QuickCheck
import Text.Printf
import Control.Monad

main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

-- 1
type MyFun a = [a] -> a
myLast ::  Eq a => MyFun a
myLast  lst = last lst
prop_1a xs x = myLast  (xs ++ [x]) == (x::String)

myLast' ::  Eq a => [a] -> a
myLast' = head . reverse
prop_1b xs x = myLast' (xs ++ [x]) == (x::String)

tests  = [("1a",                 quickCheck prop_1a)
         ,("1b",                 quickCheck prop_1b)
         ]

type PropType a = [a] -> a -> Bool
prop_1c :: Eq a => PropType a
prop_1c = \xs x -> myLast'  (xs ++ [x]) == x
prop_1d ::  Eq a => [a] -> a -> Bool
prop_1d xs x = myLast  (xs ++ [x]) == x

mkP :: Eq a => MyFun a -> PropType a
mkP f = \xs x -> f (xs ++ [x]) == x

mkProp ::  Eq a => ([a] -> a) -> [a] -> a -> Bool
mkProp f = \xs x -> f (xs ++ [x]) == x
tt xs x = quickCheck $ mkProp myLast xs x
ps xs x = map (mkProp xs x) [myLast,myLast']
ps2 :: Eq a => [MyFun a] -> [PropType a]
ps2 fs = map mkP fs 
-- pss = ps2
ffs :: Eq a => [MyFun a]
ffs = [myLast,myLast']
pss = map (mkP) ffs

-- ttt = forM_ ps $ \s-> quickCheck s
ts xs x = map (\p -> quickCheck p) (ps xs x)
tests2 xs x = zip ['a'..] (ts xs x)


