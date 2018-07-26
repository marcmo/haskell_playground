import VacuumUbigraph
import qualified Data.Map as M

data Tree a = Leaf a | Branch a (Tree a) (Tree a)

sampleTree = Branch 3 (Branch 4 (Leaf 2) (Leaf 1)) (Leaf 9)
test = view $ cycle [0..10]

test2 = view sampleTree 

tree1 :: Tree Int 
tree1 =
  Branch 1 
    (Branch 3
      (Branch 5 
        (Leaf 7)
        (Branch 6 (Leaf 4) (Leaf 1))) 
      (Branch 2
        (Leaf 3)
        (Branch 7 (Leaf 9) (Leaf 2)))) 
    (Branch 1
      (Branch 8 (Leaf 2) (Leaf 1)) (Leaf 5))

map3 n = foldl (\m x-> M.insert x x m) M.empty [0..n]

play x = let m = map3 5 in
  
