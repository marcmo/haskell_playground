module Myparse
where

import Parse
import qualified Data.ByteString.Lazy as L
import Data.Int (Int64)

data ParseState2 = ParseState2 {
      string :: L.ByteString
    , offset :: Int64           -- imported from Data.Int
    } deriving (Show)

{-- snippet Parse --}
-- newtype Parse2 a = Parse2 {
--       runParse :: ParseState2 -> Either String (a, ParseState2)
--     }
(=>>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser =>> parseFunction = Parse chained
  where chained initState =
          case runParse firstParser initState of
            Left msg -> Left msg
            Right (x, state) -> 
              let secondParser = parseFunction x in
                (runParse secondParser) state

(>=>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser >=> secondParser  =  Parse chainedParser
  where chainedParser initState   =
          case runParse firstParser initState of
            Left errMessage ->
                Left errMessage
            Right (firstResult, newState) ->
                runParse (secondParser firstResult) newState
  
--putState :: ParseState -> Parse a
--putState state = Parse (\_ -> (Right((),state))