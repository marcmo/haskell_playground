import Data.Monoid

instance (Show a) => Show (SkewHeap a) where
  show (SkewHeap x l r) = unlines [show x, (show l) ++ "-" ++ (show r)]
  show Empty = ""
instance (Ord a) => Monoid (SkewHeap a) where
  mappend = (+++)
  mempty = Empty

data SkewHeap a = Empty | SkewHeap a (SkewHeap a)(SkewHeap a)

(+++) :: (Ord a) => SkewHeap a -> SkewHeap a -> SkewHeap a
(+++) h@(SkewHeap x l r) h2@(SkewHeap x2 l2 r2)
  | x <= x2 = SkewHeap x (r +++ h2) l
  | otherwise =  SkewHeap x2 (r2 +++ h) l2
(+++) Empty h = h
(+++) h Empty = h

extractMin Empty = Nothing
extractMin (SkewHeap x l r) = Just (x, l +++ r)

testA = SkewHeap 5 (SkewHeap 3 Empty Empty) Empty
testB = SkewHeap 1 (SkewHeap 10 (SkewHeap 8 Empty Empty) Empty) Empty
testC = testA +++ testB

