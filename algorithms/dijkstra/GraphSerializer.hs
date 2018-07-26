{-# LANGUAGE OverloadedStrings #-}
module GraphSerializer(writeGraph,writeGraphRep,Link(..)) where

import Dijkstra
import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as BL

data Link = Link String String Int String
             deriving (Show)
data LinkList = LinkList [Link] deriving (Show)
data GraphRep = GraphRep [Link] [(String,String)] String String
  deriving (Show)

instance ToJSON Link where
  toJSON (Link s t len typ) = object [ "source" .= s,
                                  "target" .= t,
                                  "length" .= len,
                                  "type" .= typ]
instance ToJSON LinkList where
  toJSON (LinkList ps) = Array $ V.fromList [toJSON x | x <- ps]
instance ToJSON GraphRep where
  toJSON (GraphRep ls p s e) = object [ "edges" .= ls,
                                        "path" .= p,
                                        "start" .= s,
                                        "end" .= e]

instance FromJSON Link where
  parseJSON (Object v) = Link <$>
                         v .: "source" <*>
                         v .: "target" <*>
                         v .: "length" <*>
                         v .: "type" 
  parseJSON _          = empty

writeGraph :: Graph -> Weight -> FilePath -> IO ()
writeGraph g f p = do
  let es = edges g
  let links = [Link (show s) (show t) (f s t) "normal" | (s,t) <- es]
  BL.writeFile p (encode links)

writeGraphRep :: Graph -> [Edge] -> Vertex -> Vertex -> Weight -> FilePath -> IO ()
writeGraphRep g p start end f fp = do
  let es = edges g
  let links = [Link (show s) (show t) (f s t) (edgeType (s,t)) | (s,t) <- es]
  let thePath = [(show a,show b) | (a,b) <- p]
  let graphRep = GraphRep links thePath (show start) (show end)
  BL.writeFile fp (encode graphRep)
    where edgeType e@(s,t)
            | elem e p && s == start = "pstart"
            | elem e p && t == end = "pend"
            | e `elem` p = "onpath"
            | otherwise = "normal"
            

-- main :: IO ()
-- main = do
--   let req = decode "{\"source\": \"A\", \"target\": \"P\", \"length\": 50}":: Maybe Link
--   print req
--   let reply = Link "A" "B" 20
--   BL.putStrLn (encode reply)
  


