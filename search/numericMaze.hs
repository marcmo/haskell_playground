module Main where
import System.Environment
import Data.List
 
data Operator = AddTwo | Double | Halve
data Operator2 = GrowX | GrowY | GrowXY
 
main :: IO ()
main = do args <- getArgs
          if length args == 2
            then let [a,b] = map read args
                 in print $ solve a b
            else putStrLn "Usage: solve START TARGET"
 
apply :: Operator -> Integer -> Integer
apply AddTwo x = x + 2
apply Double x = x * 2
apply Halve x = x `div` 2

data Pos = Pos Int Int deriving (Show,Eq)
type Dim = (Int,Int)
apply2 :: Operator2 -> Pos -> Pos
apply2 GrowX (Pos x y) = (Pos (x+1) y)
apply2 GrowY (Pos x y) = (Pos x (y+1))
apply2 GrowXY (Pos x y) = (Pos (x+1) (y+1))

 
valid :: Operator -> Integer -> Bool
valid Halve x = even x
valid _ _ = True

valid2 :: Dim -> Operator2 -> Pos -> Bool
valid2 (w,h) GrowX (Pos x _) = x < w
valid2 (w,h) GrowY (Pos _ y) = y < h
valid2 (w,h) GrowXY (Pos x y) = x < w && y < h
 
solve :: Integer -> Integer -> [Integer]
solve a b = solve' [[a]] b [a]
 
solve' :: [[Integer]] -> Integer -> [Integer] -> [Integer]
solve' paths target seen =  case find ((== target) . last) paths of
                                Just path -> path
                                Nothing -> let newPaths = filter ((`notElem` seen) . last) $ concatMap buildPathsFrom paths
                                               newSeen = seen ++ map last newPaths
                                           in solve' newPaths target newSeen

 
buildPathsFrom :: [Integer] -> [[Integer]]
buildPathsFrom path = let n = last path
                      in [ path ++ [ apply operator n ] | operator <- [AddTwo, Double, Halve], valid operator n ]

buildPathsFrom2 ::  Dim -> [Pos] -> [[Pos]]
buildPathsFrom2 dim path = let n = last path
                      in [ path ++ [ apply2 operator n ] | operator <- [GrowX,GrowY,GrowXY], valid2 dim operator n ]
