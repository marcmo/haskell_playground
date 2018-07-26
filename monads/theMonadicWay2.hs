module TheMonadicWay where
data Term = Con Int
          | Add Term Term
            deriving (Show)

type MO a = (a, Out)
type Out = String

formatLine :: Term -> Int -> Out
formatLine t a = "eval (" ++ show t ++ ") <= " ++ show a ++ " - "
 
mkMO :: a -> MO a
mkMO a = (a, "")
 
bindMO :: MO a -> (a -> MO b) -> MO b
bindMO m f = (b, x ++ y)
             where (a, x) = m
                   (b, y) = f a
 
combineMO :: MO a -> MO b -> MO b
combineMO m f = m `bindMO` \_ -> f
 
outMO :: Out -> MO ()
outMO x = ((), x)
 
evalMO :: Term -> MO Int
evalMO (Con a) = outMO (formatLine (Con a) a) `combineMO`
                 mkMO a
evalMO (Add t u) = evalMO t `bindMO` \a ->
                   evalMO u `bindMO` \b ->
                   outMO (formatLine (Add t u) (a + b)) `combineMO` 
                   mkMO (a + b)
