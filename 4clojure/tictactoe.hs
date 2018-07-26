-- A tic-tac-toe board is represented by a two dimensional vector. X is represented by :x, O is represented by :o, and empty is represented by :e. A player wins by placing three Xs or three Os in a horizontal, vertical, or diagonal row. Write a function which analyzes a tic-tac-toe board and returns :x if X has won, :o if O has won, and nil if neither player has won.
import Data.List(transpose)

__ :: [[Field]] -> Maybe Field
__ rs
  | fullRow X rs = Just X
  | fullRow O rs = Just O
  | otherwise = Nothing

fullRow ::  Eq a => a -> [[a]] -> Bool
fullRow f rs = any threeInRow [rs,transpose rs,diagonals rs] where
  threeInRow xs = any (== True)[all (== f) r | r <- xs]
  diagonals xs = [[rs!!i!!i| i <- [0..2]],[xs!!(2-i)!!i| i <- reverse [0..2]]]

data Field = X | O | E deriving (Eq,Show)



testA = ((==) Nothing (__ [[E,E,E],
                           [E,E,E],
                           [E,E,E]]))
testB = ((==) (Just X) (__ [[X,E,O],
                            [X,E,E],
                            [X,E,O]]))
testC = ((==) (Just O) (__ [[E,X,E],
                            [O,O,O],
                            [X,E,X]]))
testD = ((==) Nothing (__ [[X,E,O],
                           [X,X,E],
                           [O,X,O]]))
testE = ((==) (Just X) (__ [[X,E,E],
                            [O,X,E],
                            [O,E,X]]))
testF = ((==) (Just O) (__ [[X,E,O],
                            [X,O,E],
                            [O,E,X]]))
testG = ((==) Nothing (__ [[X,O,X],
                           [X,O,X],
                           [O,X,O]]))

allTests = foldl (&&) True [testA,testB,testC,testD,testE,testF,testG]
