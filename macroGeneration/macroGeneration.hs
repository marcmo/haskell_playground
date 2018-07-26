module MacroGeneration
    where

import System.IO
import Maybe
import Char
import List
import Text.ParserCombinators.Parsec

generateUpTo :: Int -> String
generateUpTo n = concat $ map generate [1..n]

generate :: Int -> String
generate n = let xs = generateList n in
             "#define createSectionList" ++ (show n) ++ "(name," ++
                                        firstLine xs  ++ ")\\\n" ++
             generateSections xs ++
             "typedef " ++ generateTypelist xs

firstLine xs = concat $ intersperse "," $ map (\x->x ++ ","++"s_"++x) xs

generateSections = concat . map (\x->"struct "++x++" : GenericSection<s_"++x++">{};\\\n")
generateTypelist xs = (concat . map (\x->"typelist<"++x++",\\\n")) xs
                      ++ "null_typelist"
                      ++ (concat $ take (length xs) $ repeat "> ")
                      ++ "name\n"

generateList :: Int -> [String]
generateList n = map (\(Macrokey m)->m) (take n [m1..])

data Macrokey = Macrokey String
                deriving (Eq)
instance Show Macrokey where
    show (Macrokey x) = show x
m1 = Macrokey "aa"

instance Enum Macrokey where
    toEnum n = allKeys!!n
    fromEnum m = case findIndex (==m) allKeys of
                   Nothing -> 1
                   Just n -> n

allKeys = [Macrokey (x:y:[])| x <- ['a'..'z'], y <- ['a'..'z']]
