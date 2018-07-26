
data Expr = Val Int | Add Expr Expr | Mult Expr Expr
  deriving (Show)

eval (Val x) = x
eval (Add a b) = eval a + eval b
eval (Mult a b) = eval a * eval b

fold f (Val x) = f x
fold f (Add a b) = f (fold f a) + f (fold f b)
fold f (Mult a b) = f (fold f a) * f (fold f b)

eval2 = fold id

leng = sum . map (\_->1)
leng2 = sum . map (const 1)

reverse2 = foldr (\x xs-> xs ++ [x]) []


