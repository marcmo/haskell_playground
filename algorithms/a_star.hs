{-# OPTIONS_GHC -fglasgow-exts #-}

module Main where
import Control.Monad (guard, liftM2)
import Control.Monad.Instances
import Data.List (elemIndex,foldl')
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.PSQueue as Q
import Data.PSQueue(Binding(..),PSQ)

type Point = (Int, Int)
type Map = [[Char]]

find :: Char -> Map -> Point
find c m = find' 0 m
 where find' _ [] = error "Can't find tile."
       find' y (h:t)
        | Just x <- elemIndex c h = (y, x)
        | otherwise = find' (y+1) t

heuristic :: Point -> Point -> Int
heuristic (x, y) (u, v) = abs (x - u) `max` abs (y - v)

successor :: Map -> Point -> [Point]
successor m (x,y) = do u <- [x + 1, x, x - 1]
                       v <- [y + 1, y, y - 1]
                       guard (0 <= u && u < length m)
                       guard (0 <= v && v < length (head m))
                       guard (u /= x || y /= v)
                       guard (m !! u !! v /= '~')
                       return (u, v)

astar start succ end cost heur
    = astar' (S.singleton start) (Q.singleton (heur start) [start])
 where
 astar' seen q
    | Q.null q  = error "No Solution."
    | end n     = next
    | otherwise = astar' seen' q'
  where
  Just (c :-> next, dq) = Q.minView q
  n     = head next
  succs = filter (`S.notMember` seen) $ succ n
  costs = map ((+ c) . (subtract $ heur n) . liftM2 (+) cost heur) succs
  q'    = foldl' (\q (k,v) -> Q.insert k v q) dq (zip costs (map (:next) succs))
  seen' = seen `S.union` S.fromList succs

path :: [[Char]] -> [Point] -> [[Char]]
path m l = iterY m l 0
 where iterY [] _ _ = []
       iterY (h:t) l n = iterX h l n 0 : iterY t l (n+1)
       iterX [] _ _ _ = []
       iterX (h:t) l n m = (if (n,m) `elem` l then '#' else h) : iterX t l n (m+1)

doit s = unlines . path m $ astar start succ (== end) cost h
 where m     = lines s
       start = find '@' m
       end   = find 'X' m
       succ  = successor m
       h     = heuristic end
       cost (x, y) = costsM M.! (m !! x !! y)
       costsM = M.fromList [('@',1),('x',1),('X',1),('.',1),('*',2),('^',3)]

main = interact doit

