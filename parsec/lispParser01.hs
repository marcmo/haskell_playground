module LispParser01 where

import Text.ParserCombinators.Parsec
import qualified Data.Map as Map

data Expr = BlaiseInt Integer |
            BlaiseSymbol String |
            BlaiseFn ([Expr]->Expr) |
            BlaiseList [Expr]

instance Show Expr where
        show (BlaiseInt x) = show x
        show (BlaiseSymbol x) = x
        show (BlaiseFn x) = "<function>"
        show (BlaiseList x) = "(" ++ unwords (map show x) ++ ")"

x = BlaiseSymbol "hi"

parseSymbol = do f <- firstAllowed
                 r <- many (firstAllowed <|> digit)
                 return $ BlaiseSymbol (f:r)
    where firstAllowed = oneOf "+-*/" <|> letter

eval :: Expr -> Expr
eval (BlaiseInt n) = BlaiseInt n
eval (BlaiseSymbol s) = ctx Map.! s
eval (BlaiseFn f) = BlaiseFn f
eval (BlaiseList (x:xs)) = apply (eval x) (map eval xs)
    where apply :: Expr -> [Expr] -> Expr
          apply (BlaiseFn f) args = f args


blaiseMul:: [Expr] -> Expr
blaiseMul [] = BlaiseInt 1
blaiseMul (x:xs) = let BlaiseInt y = x
                       BlaiseInt rest = blaiseMul xs
                   in BlaiseInt (y*rest)

ctx = Map.fromList [("*", BlaiseFn blaiseMul)]
