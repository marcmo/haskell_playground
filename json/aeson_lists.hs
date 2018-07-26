{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative((<$>),(<*>))
import Data.Attoparsec(parse,maybeResult)
import Data.Aeson
import qualified Data.Vector as V
import qualified Data.Aeson.Types as T
import qualified Data.ByteString.Lazy.Char8 as BL

data Point = Point (Int,Int)
    deriving (Show)
instance ToJSON Point where
     toJSON (Point (a,b)) = object ["x" .= a, "y" .= b]
instance FromJSON Point where
    parseJSON (Array a) = toPair a

data PointList = PointList [Point]
    deriving (Show)
instance FromJSON PointList where
    parseJSON (Array a) = PointList <$> mapM parseJSON (V.toList a)
instance ToJSON PointList where
  toJSON (PointList ps) = Array $ V.fromList [toJSON x | x <- ps]

data PointData = PointData [PointList]
    deriving (Show)
instance FromJSON PointData where
    parseJSON (Array a) = PointData <$> mapM parseJSON (V.toList a)
instance ToJSON PointData where
  toJSON (PointData ps) = Array $ V.fromList [toJSON x | x <- ps]

toPair ::  V.Vector Value -> T.Parser Point
toPair v = Point <$> ((,) <$> parseJSON a <*> parseJSON b)
            where [a,b] = V.toList v
main = maybe (print "parse not successfull")
          (\v-> do
              let pd :: PointData
                  pd = case fromJSON v of
                          Success a -> a
                          Error s   -> error s
              print pd
              putStrLn $ "Encoded back: " ++ BL.unpack (encode pd))
          (maybeResult $ parse json testJson)

testJson = "[[[1,4893],[2,4790],[3,4684],[4,4543],[5,4401],[6,4285]],[[1,88],[2,899],[3,684],[4,543],[5,401],[6,285]]]"


