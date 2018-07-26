import Test.QuickCheck

orderedList = oneof [return [1,2,3], return [4..9]]
mygen = oneof [return 1, return 2]
orderedList2 = two mygen
-- orderedList3 n = vectorOf 8 arbitrary
-- prop_reverses = forAll (vector 9) $ \xs-> reverse (reverse xs) == xs

ordered xs = and (zipWith (<=) xs (drop 1 xs))
insert x xs = takeWhile (<x) xs++[x]++dropWhile (<x) xs

prop_Insert2 x = forAll orderedList $ \xs -> ordered (insert x xs)
  where types = x::Int
prop_Insert3 x = forAll (vector 8 arbitrary) $ \xs -> ordered (insert x xs)
  where types = x::Int
