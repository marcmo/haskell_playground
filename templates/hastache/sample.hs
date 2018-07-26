{-# LANGUAGE DeriveDataTypeable #-}
import Text.Hastache 
import Text.Hastache.Context 
import qualified Data.ByteString.Lazy as LZ 
import Data.Data 

data Info = Info { 
    name    :: String, 
    unread  :: Int 
    } deriving (Data, Typeable)

main = do 
    res <- hastacheStr defaultConfig (encodeStr template) 
        (mkGenericContext inf) 
    LZ.putStrLn res 
    where 
    template = "Hello, {{name}}!\n\nYou have {{unread}} unread messages."
    inf = Info "Haskell" 100
