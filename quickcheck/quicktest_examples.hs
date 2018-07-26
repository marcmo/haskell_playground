import Data.List
import Test.QuickCheck
import Control.Monad

type Grid      =  Matrix Char
type Matrix a  =  [Row a]
type Row a     =  [a]

cols  :: Matrix a -> [Row a]
cols m = transpose m
--cols3 :: Matrix a -> [Row a]
cols3 m = [ combineNth n m | n <- [0..(length (head m)) -1] ] where
    combineNth n m = foldr (\r acum-> r!!n:acum) [] m

validMatrix :: Matrix a -> Bool
validMatrix [] = False
validMatrix m = let headlen = length (head m) in
                foldl (\x y -> (length y == headlen) && x)
                          ((headlen > 2) && headlen == length m) m

prop_cols1 m =
    m == cols (cols m)
prop_cols2 m =
    m == transpose (transpose m)
prop_cols3 :: Eq a => Matrix a -> Property
prop_cols3 m =
    validMatrix m ==>
    collect (length m) $ map head (cols m) == head m
--     classify (null m) "trivial" $ map head (cols m) == head m
--         map head (cols m) == head m

boxs  :: Matrix a -> [Row a]
boxs = concat . map (groupCols . cols)  . split
     where groupCols = (map concat . split)
chop :: Int -> Row a -> [Row a]
chop _ [] = []
chop n r = take n r : chop n (drop n r)
split = chop 3


data Color = Red | Green | Blue
instance Arbitrary Color where
    arbitrary = oneof
                [return Red, return Blue, return Green]

data Tree a = Leaf a | Branch (Tree a) (Tree a)

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized arbTree

arbTree 0 = liftM Leaf arbitrary
arbTree n = frequency
            [(1,liftM Leaf arbitrary)
             , (4,liftM2 Branch (arbTree (n `div` 2))
                   (arbTree (n `div` 2)))]

instance Arbitrary a => Arbitrary (Matrix a) where
    arbitrary = sequence [vector 9 | i <- [1..9]]

ordered xs = and (zipWith (<=) xs (drop 1 xs))
insert1 x xs = takeWhile (<x) xs++[x]++dropWhile (<x) xs

prop_Insert x xs = ordered xs ==> ordered (insert x xs)
    where types = x::Int
prop_Insert2 x xs = ordered xs ==> null xs `trivial` ordered (insert x xs)
    where types = x::Int
prop_Insert3 x xs =
    ordered xs ==>
            classify (ordered (x:xs)) "at=head"$
            classify (ordered (xs++[x])) "at=tail"$
            ordered (insert x xs)
    where types = x::Int
prop_Insert4 x xs =
    ordered xs ==> collect (length xs)$
            ordered (insert x xs)
    where types = x::Int
prop_Insert5 x xs =
    ordered xs ==> collect (length xs)$
            classify (ordered (x:xs)) "at=head"$
            classify (ordered (xs++[x])) "at=tail"$
            ordered (insert x xs)
    where types = x::Int
prop_Insert6 x = forAll (vector 9) $ \xs -> ordered (insert x xs)
    where types = x::Int

-- matrix = sequence [vector n | i <- [1..n] ]
matrix = oneof [return ["123","456"],return ["12","34","56"]]

--prop_transpose m = forAll matrix $ \m -> (classify (length m > 2)
prop_transpose m = forAll (vector 9) $ \m -> (classify (length m > 2)
                                                       "criteria met!" $ (m == (transpose (transpose m))))
--    where types = m::Matrix Int

prop_RevApp xs ys =
    collect (length xs) $ reverse (xs ++ ys) == reverse ys ++ reverse xs
        where types = xs::[Int]
