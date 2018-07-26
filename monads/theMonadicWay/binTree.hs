module BinTree
where

data BinTree a = Leaf a |
                 Branch (BinTree a)(BinTree a) deriving Show

-- instance Show (BinTree a) where 
--     show (Leaf x) = show "Leaf " 

type M a = (Output,a)
type Output = String

data Term = Con Int | Add Term Term

eval :: Term -> Int
eval (Con a) = a
eval (Add t u) = (eval t) + (eval u)

type M2 a = State -> (a,State) 
type State = Int

eval2 :: Term -> M2 Int
--eval2 (Con a) = (\x -> (a,x))
eval2 (Con a) x = (a,x+1)
eval2 (Add t u) x = let (a,y) = (eval2 t) x in
                    let (b,z) = (eval2 u) y in
                    (a+b,z+1)

tmp = eval2 (Con 3)
tmp2 = eval2 (Add (Con 3)(Add (Con 3)(Con 1)))
exp2 = tmp2 0

tree = Branch (Leaf 3) (Branch (Leaf 2)(Leaf 8))

evenOrOdd :: Integral t => BinTree t -> M t
evenOrOdd (Leaf x) = if even x then ("even",x) else ("odd",x)
evenOrOdd (Branch lh rh) = 
    (x ++ " -- "++ y, a+b)
    where (x,a) = evenOrOdd lh
          (y,b) = evenOrOdd rh
