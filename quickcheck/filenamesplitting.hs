module Filenamesplitting

where

import Control.Monad ( liftM,liftM2,mplus )
import Data.List ( intersperse,findIndex )
import Test.QuickCheck
import System.Random
  ( RandomGen(..)
  , Random(..)
  , StdGen
  , newStdGen
  )

newtype Filename = FN String
    deriving (Show,Eq)
validCharacters = ['a'..'z']
instance Arbitrary Filename where
    arbitrary = do name <- listOf $ elements validCharacters
                   ext <- listOf $ elements ['a'..'z']
                   return (FN (name ++ "." ++ ext))
fn1 = FN "foo.txt"
splittFileName :: Filename -> (String,String)
splittFileName (FN xs) = case findIndex (=='.') xs of
                      Nothing -> (xs,"")
                      Just n -> splitAt n xs

joinFilename :: (String,String) -> Filename
joinFilename (name,ext) = FN (name ++ ext)

prop_splitt_and_merge :: Filename -> Property
prop_splitt_and_merge fn@(FN fname) =
    collect fname $
    joinFilename (splittFileName fn) == fn

prop_splitt_and_merge2 fn@(FN fname) =
    classify (length ext == 1) "no ext" $
    classify (length ext > 1 && length ext < 5) "normal ext" $
    classify (length ext >= 5) "long ext" $
    joinFilename (splittFileName fn) == fn
        where (pre,ext) = splittFileName fn

filenames :: Gen String
filenames = do
  name <- opt identifier
  dot  <- opt (return ".")
  ext  <- opt identifier
  exts <- listOf identifier
  oneof [ return $ name ++ dot ++ ext
        , return $ name ++ "." ++ (concat . intersperse "." $ exts)]
filenames2 :: Gen String
filenames2 = do
  oneof [ myplus (\x y -> x ++ y) [identifier,identifier], return "."]

myplus :: (a -> a -> a) -> [Gen a] -> Gen a
myplus op xs = map op (sequence xs)
prop_filenames_are_roundtrippable_3 :: Property
prop_filenames_are_roundtrippable_3 =
    forAll filenames $ \fn ->
    joinFilename (splittFileName (FN fn)) == FN fn

iden0 :: Gen Char
iden0 = oneof [ elements ['a'..'z'], elements ['A'..'Z']
              , elements ['0'..'9'] ]

idenN :: Gen String
idenN = listOf iden0

opt :: Gen String -> Gen String
opt g = oneof [ g, return "" ]

identifier :: Gen String
identifier = liftM2 (:) iden0 idenN

