data Term = Con Float | Div Term Term

-- instance Show a => Show (Value a) where
--     show (Result x) = "Result: " ++ show x
instance Show Term where
    show (Con x) = "(Con " ++ show x ++ ")"
    show (Div t u) = "(Div " ++ show t ++ show u ++ ")"

t1 = Div (Con 6)(Con 3)
t2 = Div (Con 1)(Con 0)
t3 = Div (Con 1)(Div (Con 6)(Con 3))

data Value a = Result a
instance Monad Value where
    return = Result
    Result x >>= q = q x

eval_with_nothing::Term->Maybe Float
eval_with_nothing(Con x) = return x
eval_with_nothing(Div t u) = do x <- eval_with_nothing t
                                y <- eval_with_nothing u
                                if y /= 0 then return (x/y) else Nothing

type M a = a

unit :: a -> M a
unit a = a

(<*>) :: M a -> (a -> M b) -> M b
a <*> k = k a

eval :: Term -> M Float
eval (Con a) = unit a
eval (Div t u) = (eval t) <*> 
                 (\a -> (eval u) <*> 
                        (\b -> (unit (a / b))))
type M5 a = a
instance Monad (M5 a) where
    return a = a
    (>>=) m f = f m


data M2 a = Raise Exception | Return a deriving (Show)
type Exception = String
unit2 :: a -> M2 a
unit2 a = Return a
(<**>) :: M2 a -> (a -> M2 b) -> M2 b
m <**> k = case m of
            Raise e -> Raise e
            Return a -> k a
raise :: Exception -> M2 a
raise e = Raise e

eval2 :: Term -> M2 Float
eval2 (Con a) = unit2 a
eval2 (Div t u) = (eval2 t) <**> 
                 (\a -> (eval2 u) <**> 
                        (\b -> (if (b == 0)
                                   then raise "divide by zero"
                                   else unit2 (a / b))))


type M3 a = State -> (a,State)
type State = Int
unit3 :: a -> M3 a
unit3 a = (\x -> (a,x))
(<***>) :: M3 a -> (a -> M3 b) -> M3 b
m <***> k = (\x -> let (a,y) = m x in
                   let (b,z) = k a y in
                   (b,z))
tick :: M3 ()
tick = (\x -> ((),x + 1))

eval3 :: Term -> M3 Float
eval3 (Con a) = unit3 a
eval3 (Div t u) = (eval3 t) <***> 
                  (\a -> (eval3 u) <***> 
                         (\b -> tick <***> (\_ -> unit3 (a / b))))



type M4 a = (Output,a)
type Output = String
line :: Term -> Float -> Output
line t a = "eval (" ++ show t ++ ") ( " ++ show a ++ "-"

unit4 :: a -> M4 a
unit4 a = ("",a)
(<+>) :: M4 a -> (a -> M4 b) -> M4 b
m <+> k = let (x,a) = m in
          let (y,b) = k a in
          (x ++ y,b)
out :: Output -> M4 ()
out x = (x ,())
eval4 :: Term -> M4 Float
eval4 (Con a) = out (line (Con a) a) <+> (\_ -> unit4 a)
eval4 (Div t u) = (eval4 t) <+> 
                 (\a -> (eval4 u) <+> 
                        (\b -> out (line (Div t u) (a/b)) <+> (\_ ->  unit4 (a/b))))

