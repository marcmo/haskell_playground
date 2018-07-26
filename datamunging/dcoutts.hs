module Main where

import Data.Char
import Data.List (intersperse)
import Data.Monoid
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy.Builder as B


convert :: B.ByteString -> B.Builder
convert = unlines' . map convertLine . C.lines

convertLine :: B.ByteString -> B.Builder
convertLine = unwords' . map convertWord . C.words

convertWord :: B.ByteString -> B.Builder
convertWord s = B.char8 (toUpper (C.head s)) <> B.lazyByteString (C.tail s)

main = do
    name <- B.readFile "lorem.txt"
    B.putStr $ B.toLazyByteString $ convert name

unwords', unlines' :: [B.Builder] -> B.Builder
unwords' = mconcat . intersperse (B.char8 ' ')
unlines' = mconcat . intersperse (B.char8 '\n')
