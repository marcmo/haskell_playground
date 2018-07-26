{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Attoparsec
import Data.Attoparsec.Number
import qualified Data.Map as M
import qualified Data.Vector as V

import qualified Data.ByteString as B
import qualified Data.Text as T

data Exif = Exif [(T.Text, ExifValue)] deriving (Show)
data ExifValue =
    ExifText T.Text |
    ExifInt Integer |
    ExifDouble Double |
    ExifBool Bool |
    ExifArray [ExifValue]
    deriving (Show)

instance FromJSON ExifValue where
    parseJSON (Number (I n)) = return $ ExifInt n
    parseJSON (Number (D n)) = return $ ExifDouble n
    parseJSON (String s)     = return $ ExifText s
    parseJSON (Bool b)       = return $ ExifBool b
    parseJSON (Array a)    = ExifArray <$> mapM parseJSON (V.toList a)

instance FromJSON Exif where
    parseJSON (Object o) = do
        x <- sequence $ map f (M.assocs o)
        return $ Exif x
        where
        f (t, x) = do
            y <- parseJSON x
            return ((t, y) :: (T.Text, ExifValue))

parseExifFile = fmap parseExifData . B.readFile

parseExifData :: B.ByteString -> Data.Attoparsec.Result (Data.Aeson.Result [Exif])
parseExifData content = parse (fmap fromJSON json) content
