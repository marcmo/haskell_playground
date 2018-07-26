module TheMonadicWay where
data Term = Con Int
          | Add Term Term
            deriving (Show)

eval :: Term -> Int
eval (Con x) = x
eval (Add x y) = eval x + eval y

type MOut a = (a, Output)
type Output = String

formatLine :: Term -> Int -> Output
formatLine t a = "eval (" ++ show t ++ ") <= " ++ show a ++ " - "

eval0 :: Term -> MOut Int
eval0 (Con a) = (a, formatLine (Con a) a)
eval0 (Add t u) = ((a + b),(x ++ y ++ formatLine (Add t u)(a+b)))
    where (a, x) = eval0 t
          (b, y) = eval0 u

bindM :: MOut a -> (a -> MOut b) -> MOut b
bindM m f = (b, x ++ y)
    where (a, x) = m
          (b, y) = f a

-- evalM_1 :: Term -> MOut Int
-- evalM_1 (Con a) = (a, formatLine (Con a) a)
-- evalM_1 (Add t u) = bindM (evalM_1 t) Add

getIntFromType typeMOut doSomething = (newInt, oldString ++ newString)
    where (oldInt, oldString) = typeMOut
          (newInt, newString) = (doSomething oldInt)

evaluator (Con a) = (a, "output-")
evaluator (Add t u) = getIntFromType (evaluator t) 
                      (\firstInt -> getIntFromType (evaluator u)
                                    (\secondInt -> ((firstInt+secondInt,"-newOutput"))))

mkM :: a -> MOut a
mkM a = (a, "")

outPut :: Output -> MOut ()
outPut x = ((), x)

evalM_2 :: Term -> MOut Int
evalM_2 (Con a) = (a, formatLine (Con a) a)
evalM_2 (Add t u) = evalM_2 t `bindM` \a -> 
                    evalM_2 u `bindM` \b ->
                    ((a + b), (formatLine (Add t u) (a + b)))

evalM_3 (Con a) = outPut (formatLine (Con a) a) `bindM` \_ -> mkM a
evalM_3 (Add t u) = evalM_2 t `bindM` \a -> 
                    evalM_2 u `bindM` \b ->
                    outPut (formatLine (Add t u)(a + b)) `bindM` \_->mkM (a+b)

-- combineM :: MOut a -> (_ -> MOut b) -> MOut b
-- combineM m f = (a, x ++ x)
--     where (a, x) = m
--           (a, x) = f a

combineM m f = m `bindM` \_-> f

evalM (Con a) = outPut (formatLine (Con a) a) `combineM` mkM a
evalM (Add t u) = evalM_2 t `bindM` \a -> 
                  evalM_2 u `bindM` \b ->
                  outPut (formatLine (Add t u)(a + b)) `combineM` mkM (a+b)
