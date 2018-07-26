module TheMonadicWay where
data Term = Con Int
          | Add Term Term
            deriving (Show)

type MO a = (a, Out)
type Out = String

formatLine :: Term -> Int -> Out
formatLine t a = "eval (" ++ show t ++ ") <= " ++ show a ++ " - "

newtype Eval_IO a = Eval_IO (a,O)
    deriving Show
type O = String

getInt monad doSomething = Eval_IO (newInt,oldOutput ++ newOutput)
    where Eval_IO (oldInt,oldOutput) = monad
          Eval_IO (newInt,newOutput) = (doSomething oldInt)

createEval_IO :: a -> Eval_IO a
createEval_IO int = Eval_IO (int,"")

print_IO string = Eval_IO ((),string)

evalM_4 :: Term -> Eval_IO Int
evalM_4 (Con a) = createEval_IO a
evalM_4 (Add t u) = evalM_4 t `getInt` \a ->
                    evalM_4 u `getInt` \b ->
                    print_IO (formatLine (Add t u) (a + b)) `getInt` \_ ->
                    createEval_IO (a + b)
