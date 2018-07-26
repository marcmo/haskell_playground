data SkewHeap a = Empty | SkewNode a (SkewHeap a) (SkewHeap a) deriving (Show)

(+++) :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
heap1@(SkewNode x1 l1 r1) +++ heap2@(SkewNode x2 l2 r2) 
  | x1 <= x2	= SkewNode x1 (heap2 +++ r1) l1 
  | otherwise = SkewNode x2 (heap1 +++ r2) l2
Empty +++ heap = heap
heap +++ Empty = heap

extractMin Empty = Nothing
extractMin (SkewNode x l r ) = Just (x , l +++ r )

test = foldl (\acc x->acc +++ x) Empty nodes
  where nodes = map (\x-> SkewNode x Empty Empty) [3,5,1,9,7,2]

