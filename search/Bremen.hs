module Bremen where

import TreeTraversal
import List

data Direct = R | L deriving (Eq,Show)
data Participant = Hund | Katze | Esel | Hahn deriving (Eq,Show)
time Hund = 5
time Katze = 10
time Esel = 20 
time Hahn = 25 
type Group = [Participant]
type Move = [Participant]
type LabyrintState = (Direct,Group)
team = [Hund,Katze,Esel,Hahn]

possiblePairs :: Group -> [(Participant,Participant)]
possiblePairs xs = [ (x,y) | x <- xs , y <- xs, (time y) > (time x) ]

duration :: [Move] -> Integer
duration = foldl (+) 0 . map (maximum . map time)

instance TreeTraversal LabyrintState Move where
    nextMoves (R,[]) = []
    nextMoves (L,group)  = [([x,y], (R,group\\[x,y])) | (x,y) <- (possiblePairs group)]
    nextMoves (R,group)  =  [([x], (L,group ++ [x])) | x <- (team\\group)]

instance TreeTraversalE Move where
    stillGood moves = duration moves <= 60

getAll :: [[Move]]
getAll = allPaths (L,team)
