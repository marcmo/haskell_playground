module Main where

import Control.Monad
import Control.Monad.Par
import Data.Functor
import Data.List as List
import System.Environment

-- Generation of solution space.

data Tree a = Node { var :: Row, info :: a, choices :: [(Column, Tree a)] }
            | Done a
  deriving Show

type Row       = Int
type Column    = Int
type BoardSize = Int
type Board     = [(Row, Column)]

-- Generate the decision tree.
generate :: BoardSize -> Tree ()
generate n = go 0
  where
    go :: Row -> Tree ()
    go r | r >= n    = Done ()
         | otherwise = Node r () [(c, go (r + 1)) | c <- [0 .. n - 1]]

-- Show a board of a given size.
showBoard :: BoardSize -> Board -> String
showBoard n b = unlines (map showRow [0 .. n - 1])
  where
    showRow r =
      case lookup r b of
        Nothing -> replicate n '.'
        Just c  -> replicate c '.' ++ "Q" ++ replicate (n - c - 1) '.'

-- Annotate a tree with the (partial) board at each node.
annotateWithBoard :: Tree () -> Tree Board
annotateWithBoard = go []
  where
    go :: Board -> Tree () -> Tree Board
    go b (Node r _ ts) = Node r b (map (\ (c, t) -> (c, go ((r, c) : b) t)) ts)
    go b (Done _)      = Done b

-- Remove all parts of the tree that contain illegal choices.
filterAll :: Tree Board -> Tree Board
filterAll (Node r b ts) =
  Node r b (map (\ (c, t) -> (c, filterAll t)) (filter (\ (c, t) ->
    not (c `elem` map snd b) && and [ abs (r - r') /= abs (c - c') | (r', c') <- b ]) ts
    ))
filterAll t             = t

-- Find the first solution in a tree, by exploring it depth-first left-to-right.
findFirst :: Tree Board -> Maybe Board
findFirst (Node _ _ cs) = msum (map (findFirst . snd) cs)
findFirst (Done b)      = Just b

-- Explore the tree interactively (not very robust; just for fun).
exploreInteractively :: BoardSize -> Tree Board -> IO ()
exploreInteractively n o@(Node r b cs) = do
  putStr (showBoard n b)
  print (zip (repeat r) (map fst cs))
  r <- readLn
  case lookup r cs of
    Just t  -> playInteractively n t
    Nothing -> do
      putStrLn "Wrong choice."
      playInteractively n o
playInteractively n (Done b) = do
  putStrLn "Success!"
  putStr (showBoard n b)

-- Count the solutions.
solutions :: Tree a -> Int
solutions (Node _ _ ts) = sum $ map (solutions . snd) ts
solutions (Done _)      = 1

-- Exercise:
--
-- Define a function
--
-- solutionsPar :: Int -> Tree a -> Par Int
--
-- that takes a depth threshold as its first argument.
-- The top layers of the tree should be traversed in parallel,
-- the lower layers of the tree should be traversed sequentially.
--
-- Change the main program to use solutionsPar.

-- Main program.
main :: IO ()
main = do
  [n] <- getArgs
  print $ solutions $ filterAll $ annotateWithBoard $ generate (read n)
