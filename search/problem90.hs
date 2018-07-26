module Problem90 where
import TreeTraversal
import List

type Row = Int
type Board = [Int]
type GameState = (Row,Board)

instance TreeTraversal GameState Row where
    nextMoves (7,_) = []
    nextMoves (n,b) = [(m,(n+1,m:b)) | m <- [1..6]]

instance TreeTraversalE Row where
--    stillGood moves = isSafe (head moves) (tail moves)
    stillGood moves = not (conflicting (tail moves) (head moves))

touchesDiagonals :: Row -> Board -> Bool
touchesDiagonals column rows = 
    let coords = zip [1..] rows
        newCoord = (length rows + 1,column)
        onDiag (r,c)(nr,nc) = abs(r-nr) == abs(c-nc)
    in any (\(row,col) -> onDiag (row,col) newCoord) coords

isSafe :: Row -> Board -> Bool
isSafe   try qs = not (try `elem` qs || sameDiag try qs)
sameDiag try qs = any (\(colDist,q) -> abs (try - q) == colDist) $ zip
                  [1..] qs

conflicting :: [Row] -> Row -> Bool
conflicting rows n =  (elem n rows) || touchesDiagonals n rows

getAll :: GameState -> [[Row]]
getAll = allPaths-- (0,[1])

queens :: Int -> [[Int]]
queens n = map reverse $ queens' n
    where queens' 0       = [[]]
          queens' k       = [q:qs | q <- [1..n],qs <- queens' (k-1),  isSafe q qs]
--          queens' k       = [q:qs | qs <- queens' (k-1), q <- [1..n], isSafe q qs]
--           queens' k       = [q:qs | qs <- queens' (k-1), q <- [1..n],
--                                           not(conflicting (reverse qs) q)]
          isSafe   try qs = not (try `elem` qs || sameDiag try qs)
          sameDiag try qs = any (\(colDist,q) -> abs (try - q) == colDist) $ zip [1..] qs

test = conflicting  [1,5,8,6,3,7,2]
test2 = (\x -> isSafe x [1,5,8,6,3,7,2])
test_ = [conflicting (init sol) x |  x <- [1..8], sol <- queens 8]
test2_ = [isSafe x (init sol) |  x <- [1..8], sol <- queens 8]

queens2 :: Int -> [[Int]]
queens2 n = filter test (generate n)
    where generate 0      = [[]]
          generate k      = [q : qs | q <- [1..n], qs <- generate (k-1)]
          test []         = True
          test (q:qs)     = isSafe q qs && test qs
          isSafe   try qs = not (try `elem` qs || sameDiag try qs)
          sameDiag try qs = any (\(colDist,q) -> abs (try - q) == colDist) $ zip [1..] qs
