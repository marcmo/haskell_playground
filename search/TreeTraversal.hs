module TreeTraversal where

import List

class TreeTraversalE move where
    stillGood :: [move] -> Bool

class (Eq node,TreeTraversalE move) => TreeTraversal node move | node -> move where
    nextMoves    :: node -> [(move,node)]
    allPaths     :: node -> [[move]]

    allPaths n = if (0 == (length (nextMoves n))) then [[]]
                 else [m:mm | (m,nn) <- nextMoves n, mm <- allPaths nn, stillGood (m:mm)]
