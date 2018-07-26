{-# LANGUAGE ViewPatterns #-}

dropPrefix :: Eq a => [a] -> [a] -> ([a],[a])
dropPrefix left@(x:xs) right@(y:ys)
    | x == y    = dropPrefix xs ys
dropPrefix left right = (left,right)

genbankHeader (dropPrefix ">gi|" -> ("", rest)) = foo rest

foo = undefined

