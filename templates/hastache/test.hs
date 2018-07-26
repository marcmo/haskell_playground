{-# LANGUAGE DeriveDataTypeable #-}
module Tests where

import Control.Monad
import Control.Monad.Writer
import Data.Char
import Data.Data
import Data.Generics
import Test.HUnit
import Text.Hastache
import Text.Hastache.Context
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LZ
import qualified Data.Text as T

-- Render external (partial) template file
partialsTest = do
    let config = defaultConfig {muTemplateFileDir = (Just "tmp")}
    res <- hastacheStr config (encodeStr template) 
        (mkStrContext context)
    print (decodeStrLBS res)
    where
    template = "text 1\n{{> partFile}}\nhuhu {{name}} 2\n"
    context "name" = MuVariable "Neo"
    

main = partialsTest


