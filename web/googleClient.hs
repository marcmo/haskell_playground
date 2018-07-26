{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
login = "https://www.google.com/accounts/ClientLogin"
main = simpleHttp "http://www.haskell.org/" >>= L.putStr
-- test = do
--     initReq <- parseUrl login
--     let req = initReq
--               { method = "POST"
--               }
--     return $ queryString req
    
main2 = do
    request <- parseUrl "http://google.com/"
    withManager $ \manager -> do
        Response _ _ _ src <- http request manager
        print ,src"




