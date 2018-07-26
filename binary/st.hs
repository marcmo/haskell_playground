import Data.Binary.Get
import Data.Word
import Control.Monad

data MsgString = Definition_msg {
      msg_no        :: Word16
    } deriving (Show)

parseDef :: Get MsgString
parseDef = do
    msg_no   <- getWord16le
    return $ Definition_msg msg_no

parseMain ::  Get [MsgString]
parseMain =  do
      bit <- getWord8
      msg <- parseDef
      msg2 <- parseDef
      return $ msg:[msg2]
      -- return $ msg ++ msg2
      -- return $ (liftM (:)) msg parseMain

