module Perm
where
import List
data Participant = Participant 
    { time :: Integer,
      name :: String } deriving Eq
data Direct = ToLeft | ToRight deriving Eq
instance Show Direct where
    show ToLeft = "<-"
    show ToRight = "->"
data Move = Move 
    { direction :: Direct,
      who :: Group } deriving Eq
instance Show Move where show m = unlines[show (direction m) ++ " " ++  show (who m)]
type Group = [Participant]
team = [Participant 5 "Hund",Participant 10 "Katze",Participant 20 "Esel",Participant 25 "Hahn"]
instance Show Participant where show p = name p
possiblePairs :: Group -> [(Participant,Participant)]
possiblePairs xs = [ (x,y) | x <- xs , y <- xs, (time y) > (time x) ]

duration :: [Move] -> Integer
duration = foldl (+) 0 . map (maximum . map time . who)

getSolution :: Group -> Integer -> [[Move]]
getSolution from minTime = filter (\x -> duration x <= minTime) (goForth from [])

goForth :: Group -> Group -> [[Move]]
goForth from to = 
    (concat (map (\(x,y) -> 
          (map (\moves -> (Move ToRight [x,y]):moves)(goBack(from\\[x,y])(x:y:to))))
     (possiblePairs from)))

goBack :: Group -> Group -> [[Move]]
goBack [] to = [[]]
goBack from to = 
    (concat 
     (map (\x -> 
           (map (\moves -> (Move ToLeft [x]):moves)(goForth(x:from)(delete  x to))))
      to))

