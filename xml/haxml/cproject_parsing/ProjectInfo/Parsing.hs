module ProjectInfo.Parsing 

where 

import ProjectInfo.ParserUtil
import qualified Text.Parsec.ByteString as PBS
import Data.ByteString

extractIncludePath ::  ByteString -> Either ParseError String
extractIncludePath line = parse p_include "" line
  where p_include = string "#include" *> spaces *> char '"' *> p_path <* char '"'
        p_path = (++) <$> (many1 $ choice [letter, char '/', char '\\', char '_']) <*> string ".h"

parseProjectName ::  String -> Either ParseError String
parseProjectName input = parse line "" input
  where line = optional (string "\"") *> ((string "${workspace_loc:/") <|> (string "../../")) 
                *> many1 (alphaNum <|> char '_') <* many1 anyChar

