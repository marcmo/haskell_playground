module MyTypes
    where

data Pair a b = Pair a b

data Triple a b c = Triple a b c

tripleFst (Triple a b c) = a
tripleSnd (Triple a b c) = b
tripleThr (Triple a b c) = c

data Quadruple a b = Quadruple a a b b

firstTwo :: Quadruple a b -> [a]
firstTwo (Quadruple x y z u) = [x,y]

lastTwo :: Quadruple a b -> [b]
lastTwo (Quadruple _ _ z u) = [z,u]

firstElement [] = Nothing
firstElement (x:xs) = Just x

findElement :: (a -> Bool) -> [a] -> Maybe a
findElement p [] = Nothing
findElement p (x:xs) = 
    if p x then Just x 
    else findElement p xs

data Tuple a b c d = One a
                   | Two a b
                   | Three a b c
                   | Four a b c d
                     deriving (Show)

-- instance Show Tuple where
--     show One a = "aaaaa"
--     show Two a b = "aaabbbbbb..."
--     show Three a b c = "abc"
--     show Four a b c d = "abcd"

tuple1 (One a) = a
tuple1 (Two a b) = a

fromTuple (One a ) = Left (Left a )
fromTuple (Two a b ) = Left (Right (a,b) )
fromTuple (Three a b c ) = Right (Left (a,b,c) )
fromTuple (Four a b c d) = Right (Right (a,b,c,d))

mytuple = One 2

data List a = Nil
            | Cons a (List a)
              deriving (Show)

mylist = Cons 1 (Cons 2 (Nil))

listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs

listHead (Cons x xs) = x

listTail (Cons x xs) = xs

listFoldl f y Nil = y
listFoldl f y (Cons x xs) = listFoldl f (f y x) xs

data BinaryTree a
    = Leaf a
    | Branch (BinaryTree a) a (BinaryTree a)
      deriving (Show)
treeSize (Leaf x) = 1
treeSize (Branch left x right) =
    1 + treeSize left + treeSize right
mytree = Branch (Leaf 1) 2 (Branch (Leaf 4) 3 (Leaf 5))
elements (Leaf x) = [x]
elements (Branch lhs x rhs) = elements lhs ++ [x] ++ elements rhs

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f y (Leaf x) = f x y
foldTree f y (Branch lhs x rhs) = 
    foldTree f (f x (foldTree f y rhs)) lhs

elements2 = foldTree (:) []
