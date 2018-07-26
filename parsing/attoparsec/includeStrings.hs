{-# LANGUAGE OverloadedStrings #-}
import Data.Attoparsec.Text(string,parse,satisfy,feed,IResult(Done))
import Control.Applicative
import qualified Data.List as L
import qualified Data.Text as T

includeParser = a <|> b
    where a = makeParser ("<",">")
          b = makeParser ("\"","\"")
          makeParser (x,y) = string "#include " *> between (string x) (string y) (many $ satisfy isIncludeChar)
          isIncludeChar c = c `L.elem` '/':['a'..'z'] ++ '.':['A'..'Z'] ++ ['0'..'9']
          between p q r = p *> r <* q
parseInclude ::  T.Text -> String
parseInclude input = r 
  where Done _ r = feed (parse includeParser input) ""
