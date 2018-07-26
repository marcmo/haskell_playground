module Prettify where

import SimpleJSON

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double d = text (show d)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where best col (d:ds) =
              case d of
                Empty        -> best col ds
                Char c       -> c :  best (col + 1) ds
                Text s       -> s ++ best (col + length s) ds
                Line         -> '\n' : best 0 ds
                a `Concat` b -> best col (a:b:ds)
                a `Union` b  -> nicest col (best col (a:ds))
                                           (best col (b:ds))
                                           width
          best _ _ = ""

nicest col a b width | (width - least) `fits` a = a
                     | otherwise                = b
    where least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show)
(<>) :: Doc -> Doc -> Doc
Empty <> b = b
a <> Empty = a
a <> b = a `Concat` b

char :: Char -> Doc
char c = Char c
line :: Doc
line = Line
hcat :: [Doc] -> Doc
hcat xs = foldr (<>) Empty xs

fsep :: [Doc] -> Doc
fsep xs = foldr (</>) Empty xs
(</>) :: Doc -> Doc -> Doc
a </> b = a <> softline <> b
softline :: Doc
softline = group line
group :: Doc -> Doc
group x = flatten x `Union` x
flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds
punctuate p _      = []
compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) = 
              case d of 
                Empty -> transform ds
                Char c -> c:transform ds
                Text t -> t ++ transform ds
                Line -> '\n' : transform ds
                Concat m n -> transform (m:n:ds)
                _ `Union` v -> transform (v:ds)


