data Tree a
    = Leaf a
    | Branch (Tree a) (Tree a)

instance Show Tree where
    show (Leaf x) = "leaf-" ++ x
    show (Branch lhs rhs) = "(Branch " ++ show lhs ++ " " ++ show rhs

mapTreeState :: (a -> state -> (state, b)) -> Tree a -> state -> (state, Tree b)
mapTreeState f (Leaf x) state = 
    let (state', y) = f x state
    in (state', Leaf y)
mapTreeState f (Branch lhs rhs) state = 
    let (state', a) = mapTreeState f lhs state
        (state'', b) = mapTreeState f rhs state'
    in (state'', Branch a b)

t = Branch (Leaf 3)(Branch (Leaf 4)(Leaf 5))

--numberTree :: Tree a -> state -> (state, Tree b)
--numberTree (Leaf x) num = (num + 1, Leaf x)
--numberTree (Branch lhs rhs) num =
  --  let
