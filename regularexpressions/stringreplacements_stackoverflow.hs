-- {-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Data.List(foldl')

type M = [(String, String)]
m = [("x", "a car"), ("y", "a animal")]
s = "this is x and y"

replace :: String -> (String,String) -> String
replace s (a,b) = let [ss,aa,bb] = [T.pack x | x <- [s,a,b]] in
  T.unpack $ T.replace aa bb ss

strRep :: M -> String -> String
strRep m input = foldl' replace input m

strRep2 = flip $ foldl' replace


test = print $ strRep2 m s

