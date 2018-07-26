module Main
    where

import MacroGeneration
import System( getArgs )

main :: IO ()
main = do args<-getArgs
          writeFile "output.txt" $ generateUpTo $ read (head args)
          return ()
