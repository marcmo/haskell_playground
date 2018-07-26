module Main 
    where

import Control.Monad.Writer

bind f x seed = let (x',seed') = x seed in f x' seed'

foo5 = return 7 >>= (\x -> Writer (x+1,"inc."))
       >>= (\x -> Writer (2*x,"double."))
       >>= (\x -> Writer (x-1,"dec."))

foo6 = do
  let x = 7
  y <- Writer (x+1,"inc\n")
  z <- Writer (2*y,"double\n")
  Writer (z-1,"dec\n")

bar = runWriter foo5
bar2 = runWriter foo5

