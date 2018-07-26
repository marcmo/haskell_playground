module CvsParsing
where

import Text.CSV.ByteString
import qualified Data.ByteString as BS
import Maybe

test = do
  input <- BS.readFile "test.csv"
  let p = parseCSV input :: Maybe CSV
  -- return p $ fromJust p 
  print "ok"

getCsv input =
  let p = parseCSV input in
    if (isJust p) then fromJust p else []
