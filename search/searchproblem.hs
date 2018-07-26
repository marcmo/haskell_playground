module SearchProblem
where
import List

class Node a where
    possibleNextSteps :: a -> [Step]

data Side = LeftSide | RightSide deriving Eq
data Step a = Step
    { start :: a,
      dest :: a } deriving Eq
data Participant = Participant 
    { time :: Integer,
      name :: String } deriving Eq
type Group = [Participant]
data PuzzleState = PuzzleState
    { groupLeft :: Group,
      groupRight :: Group,
      side :: Side}
instance Node PuzzleState where
    possibleNextSteps sit = 
        map (\node -> Step (PuzzleState groupLeft groupRight LeftSide) node) possibleNextNodes
        where possibleNextNodes =
                  if ((side sit) == LeftSide) then
                      map (\(x,y) -> (PuzzleState (groupLeft\\[x,y])(x:y:groupRight) RightSide)) possiblePairs (groupLeft sit)
                  else map (\x -> (PuzzleState (groupLeft\\[x])(x:groupRight) LeftSide)) groupRight
            
type Path = [Step]

possiblePairs :: Group -> [(Participant,Participant)]
possiblePairs xs = [ (x,y) | x <- xs , y <- xs, (time y) > (time x) ]

empty :: [a] -> Bool
empty [] = True
empty _ = False

findAllPaths :: Node -> [Path]
findAllPaths k = 
    let nextSteps = possibleNextSteps k
    in map f nextSteps
    where f step = if (empty nextPaths) then [step]
                   else step:nextPaths
                       where nextPaths = findAllPaths (dest step)
    

