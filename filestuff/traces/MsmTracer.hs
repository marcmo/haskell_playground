{-# LANGUAGE OverloadedStrings #-}

module Main
       where

-- 0.0013 2  1A6             Rx   d 8 00 00 00 00 00 00 80 F0
import qualified Data.ByteString.Char8 as BC
import Data.Attoparsec
import Data.Attoparsec.Char8
import Data.Word

import qualified Data.Text as T

data CanMessage = CanMessage { canId :: T.Text,
                             timeStamp :: Int
                           } deriving (Eq, Show)

x ::  Parser Word8
x = char8 'a'
timestamp ::  Result Number
timestamp  = do
  let p@(Partial f) = parse number "0.1"
  feed p BC.empty

-- timestampAndId s = do
--   let p = parse fff s
--   feed p BC.empty

fff ::  Parser (Number, Number, Int)
fff = do
  n <- number
  many1 space
  n2 <- number
  many1 space
  _id <- hexadecimal
  many1 space
  many1 letter_ascii

  return (n,n2,_id)

test s = feed (parse fff s) BC.empty

main = do
  -- parseTest x "abc"
  -- parseTest timestamp "0.0013"
  let (Partial f) = parse number "1"
  let res = f ""
  print res

