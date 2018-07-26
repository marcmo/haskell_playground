#!/usr/local/bin/runhaskell
import Text.Hastache 
import Text.Hastache.Context 
import qualified Data.ByteString.Lazy as LZ 

main = hastacheStr defaultConfig (encodeStr template) (mkStrContext context)
    >>= LZ.putStrLn

-- begin example
template = "Hello, {{#reverse}}world{{/reverse}}!" 

context "reverse" = MuLambda (id . decodeStr)


