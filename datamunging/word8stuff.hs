module Main where

import Data.Word8 (Word8, _cr, _nbsp, _space, _tab, isAsciiLower)
import qualified Data.ByteString as B

isSpace :: Word8 -> Bool
isSpace w = w == _space || w == _nbsp || w <= _cr && w >= _tab

toUpper :: Word8 -> Word8
toUpper w = if isAsciiLower w then w - _space else w

op :: Bool -> Word8 -> (Bool, Word8)
op flag c = if isSpace c then (True,c) else (False,d)
    where d = if flag then toUpper c else c

main :: IO ()
main = B.readFile "lorem.txt" >>= B.putStr . snd . B.mapAccumL op True

