{-# LANGUAGE OverloadedStrings #-}

module Main
       where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Attoparsec

import qualified Data.Vector as V
import qualified Data.ByteString as B
import qualified Data.Text as T

data ExifEntry = ExifEntry { exifMake :: T.Text,
                             exifModel :: T.Text,
                             exifOrientation :: T.Text,
                             exifXResolution :: Int,
                             exifYResolution :: Int,
                             exifResolutionUnit :: T.Text
                           } deriving (Eq, Show)


instance FromJSON ExifEntry
  where
    parseJSON (Object v) = (ExifEntry <$> v .: "EXIF:Make" <*>
                                          v .: "EXIF:Model" <*>
                                          v .: "EXIF:Orientation" <*>
                                          v .: "EXIF:XResolution" <*>
                                          v .: "EXIF:YResolution" <*>
                                          v .: "EXIF:ResolutionUnit")
    parseJSON _          = mzero

parseAll :: B.ByteString -> [ExifEntry]
parseAll s = case (parse (fromJSON <$> json) s) of
  Done _ (Error err)  -> error err
  Done ss (Success e) -> e:(parseAll ss)
  _                   -> []

main :: IO ()
main = do s <- B.readFile "sample.json"
          let p = parseAll s
          print p

