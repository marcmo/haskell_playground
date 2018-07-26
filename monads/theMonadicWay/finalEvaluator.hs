module Test where

import Control.Monad

data Result a
    = Good a State Output
    | Bad State Output
    deriving Show

newtype Eval_SOI a = SOIE {runSOIE :: State -> Result a}

type State = Int
type Output = String

raise e = SOIE (\s -> Bad s e)

instance Monad Eval_SOI where
    return a = SOIE (\s -> Good a s "")

    m >>= f = SOIE $ \x ->
        case runSOIE m x of
            Good a y o1 ->
                case runSOIE (f a) y of
                    Good b z o2 -> Good b z (o1 ++ o2)
                    Bad z o2 -> Bad z (o1 ++ o2)
            Bad z o2 -> Bad z o2  -- (*)


display t = SOIE(\s -> Good () s t)

test = runSOIE (do
   display "hello"
   raise "Exception"
   display "Foo"
  ) 0
