import Data.ByteString.Base64
import System.Environment
import qualified Data.ByteString as B
import Data.ByteString.Char8(pack)
import qualified Data.List as L
import Numeric (showHex)
import Data.Word
import qualified Data.ByteString.Char8 as BC

main = do
  let sps = [0x00, 0x00, 0x00, 0x01, 0x67, 0x42, 0x00, 0x0a, 0xf8, 0x41, 0xa2]
  let pps = [0x00, 0x00, 0x00, 0x01, 0x68, 0xce, 0x38, 0x80]
  [input] <- getArgs
  putStrLn $ "original:" ++ input
  putStrLn $ base64AsString input
  putStrLn $ "converted from byte-array:" ++ (asBase64 [0x28,0xde,0x9,0x88])
  putStrLn $ "converted from byte-array sps:" ++ (asBase64 sps)
  putStrLn $ "converted from byte-array pps:" ++ (asBase64 pps)

base64AsString ::  String -> String
base64AsString s = either id (prettyPrint) (decode $ pack s)

prettyPrint :: B.ByteString -> String
prettyPrint = (L.intercalate ",") . map (\x->"0x" ++ showHex x "") . B.unpack

asBase64 :: [Word8] -> String
asBase64 = BC.unpack . encode . B.pack


