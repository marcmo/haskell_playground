-- type Env = GenEnv Value
data Exp = Num Integer
         | Prim Op [Exp]
         | Exp :+ Exp
         | Exp :* Exp deriving (Eq,Show)

data Op = IntAddOp deriving (Eq,Show)

data Value
    = IVal Integer
    | BVal Bool

eval :: Exp -> Value
eval  (Num n) = IVal n
eval  (Prim IntAddOp ((Num x) : (Num y):[])) = IVal (x+y)
eval  (Prim IntAddOp (x:xs)) =  IVal  ((IVal (eval x)) + (IVal (eval (Prim IntAddOp xs))))
-- eval  (Prim IntAddOp (x:xs)) =  IVal  ((IVal (eval x)) + (IVal (eval (Prim IntAddOp xs))))

