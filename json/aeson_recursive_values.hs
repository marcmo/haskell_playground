{-# LANGUAGE OverloadedStrings #-}
import Data.Maybe(fromJust)
import Data.ByteString as BS
import Control.Applicative((<$>),(<*>))
import qualified Data.HashMap.Strict as HM
import Data.Attoparsec(parse,maybeResult)
import qualified Data.Attoparsec.Number as N
import Data.Aeson
import qualified Data.Vector as V
import qualified Data.Aeson.Types as T

data Data = D1 Int | D2 [Data]
    deriving (Show)

instance FromJSON Data where
    parseJSON (Number (N.I n)) = return $ D1 $ fromInteger n
    parseJSON (Array a) = D2 <$> mapM parseJSON (V.toList a)

data Request = Incoming String String String
             | Outgoing String String String deriving (Show,Eq)
instance ToJSON Request where
    toJSON (Incoming a b c) = object ["response" .= object ["source" .= a, "target" .= b, "response" .= c]]
    toJSON (Outgoing a b c) = object ["request" .= object ["source" .= a, "target" .= b, "response" .= c]]
-- data Request = Request String deriving (Show)
-- instance ToJSON Request where
--      toJSON (Request r) = object ["request" .= r]
-- instance FromJSON Request where
--     parseJSON (Object v) = Request <$> v .: "request"

data ServerState = ServerState Bool [Request] deriving (Show)

instance FromJSON ServerState where
    parseJSON (Object v) = ServerState <$> v .: "connected" <*> v .: "output"
instance FromJSON Request where
    parseJSON j = do
      o <- parseJSON j -- takes care of JSON type check
      case HM.toList (o :: Object) of
        [("response", Object r)] -> Incoming <$> r .: "source" <*> r .: "target" <*> r .: "response"
        [("request", Object r)] -> Outgoing <$> r .: "source" <*> r .: "target" <*> r .: "response"
        _                      -> fail "Rule: unexpected format"

main = do
    let v = fromJust $ maybeResult $ parse json testJson
    let v1 :: Data
        v1 = case fromJSON v of
                 Success a -> a
                 Error s   -> error s
    print v1

testJson :: BS.ByteString
testJson = "[1,2,3,[5,3,[6,3,5]]]"
testMe ::  Value
testMe = fromJust $ maybeResult $ parse json testJson
testState :: BS.ByteString
testState = "{\"connected\":true, \"output\":[{\"response\":{\"response\":\"abc\",\"source\":\"a\",\"target\":\"b\"}},{\"request\":{\"response\":\"ABC\",\"source\":\"A\",\"target\":\"B\"}}]}"
testMe2 = fromJust $ maybeResult $ parse json testState
-- reqs = [Request "hello", Request "World"]
reqs = [Incoming "a" "b" "abc", Outgoing "A" "B" "ABC"]


