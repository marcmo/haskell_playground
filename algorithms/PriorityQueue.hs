module PriorityQueue (
        PriorityQueue,
        empty,
        singleton,
        fromList,
        null,
        deleteFindMin,
        deleteMin,
        findMin,
        insert,
        union
       ) where
 
import Prelude hiding (null)
import Data.List (foldl')
 
data Ord k => PriorityQueue k a = Nil | Branch k a (PriorityQueue k a) (PriorityQueue k a)
 
empty :: Ord k => PriorityQueue k a
empty = Nil
 
singleton :: Ord k => k -> a -> PriorityQueue k a
singleton k a = Branch k a Nil Nil
 
fromList :: Ord k => [(k,a)] -> PriorityQueue k a
fromList = foldl' (\q (k,a) -> singleton k a `union` q) empty
 
null :: Ord k => PriorityQueue k a -> Bool
null Nil = True
null _   = False
 
deleteFindMin :: Ord k => PriorityQueue k a -> ((k,a), PriorityQueue k a)
deleteFindMin Nil = error "Empty heap."
deleteFindMin (Branch k a l r) = ((k,a), union l r)
 
deleteMin :: Ord k => PriorityQueue k a -> PriorityQueue k a
deleteMin h = snd (deleteFindMin h)
 
findMin :: Ord k => PriorityQueue k a -> (k, a)
findMin h = fst (deleteFindMin h)
 
insert :: Ord k => k -> a -> PriorityQueue k a -> PriorityQueue k a
insert k a h = union (singleton k a) h
 
union :: Ord k => PriorityQueue k a -> PriorityQueue k a -> PriorityQueue k a
union l Nil = l
union Nil r = r
union l@(Branch kl _ _ _) r@(Branch kr _ _ _)
    | kl <= kr  = link l r
    | otherwise = link r l
 
link (Branch k a Nil m) r = Branch k a r m
link (Branch k a ll lr) r = Branch k a Nil (union (union r ll) lr)

