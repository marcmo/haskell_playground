{-# LANGUAGE DatatypeContexts #-}
import qualified Data.Map as M
import Data.Maybe
import Prelude hiding (lookup)

data Ord a => Trie a b = MkTrie (Maybe b) (M.Map a (Trie a b))

instance (Ord a, Show a, Show b) => Show (Trie a b) where
  show (MkTrie x m) = show x ++ "\n" ++ M.showTree m
  -- show (MkTrie x m) = show x ++ "\\n" ++ M.showTreeWith (\\k x -> show (k,x)) True True m

lookup ::  (Ord a) => [a] -> Trie a b -> Maybe b
lookup [] (MkTrie b _) = b
lookup (x:xs) (MkTrie _ m) = M.lookup x m >>= lookup xs

empty :: Ord a => Trie a b
empty = MkTrie Nothing M.empty

bind :: Ord a => [a] -> b -> Trie a b -> Trie a b
bind [] x (MkTrie _ m) = MkTrie (Just x) m
bind (k:ks) x (MkTrie b m) =
  let t = maybe empty id (M.lookup k m)
      t' = bind ks x t in
    MkTrie b (M.insert k t' m)

size :: Ord a => Trie a b -> Int
size (MkTrie b m) = let s = maybe 0 (const 1) b in
    s + M.fold (\x acc->acc + size x) 0 m

nodes :: Ord a => Trie a b -> Int
nodes (MkTrie b m) = let s = if isJust b && (not $ M.null m) then 1 else 0 in
    s + M.fold (\x acc->acc + nodes x) 0 m


