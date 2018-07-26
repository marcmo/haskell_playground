module SimpleMessage where

import Foreign(Word8)
import Data.Serialize
import Data.List
import Text.Printf
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

data SimpleMessage = SimpleMessage {
  payloadLen :: Int,
  payload :: [Word8]
} deriving (Eq)
instance Show SimpleMessage where
  show (SimpleMessage len p) =
    show len ++ " - " ++ showAsHexString p

instance Serialize SimpleMessage where
  put m = do
    putWord32be $ fromIntegral $ payloadLen m
    putByteString $ S.pack $ payload m
  get = do
    len <- getWord32be
    payload <- getBytes (fromIntegral len)
    return $ SimpleMessage (fromIntegral len) (S.unpack payload)

dataMessage :: [Word8] -> SimpleMessage
dataMessage xs = SimpleMessage (length xs) xs

showAsHexString ::  [Word8] -> String
showAsHexString bs = '[':(intercalate "," $ map (printf "0x%02x") bs) ++ "]"

showBinString ::  S.ByteString -> String
showBinString xs = showAsHexString $ S.unpack xs

