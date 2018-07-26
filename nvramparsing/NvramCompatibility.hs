module NvramCompatibility where
  
-- TODO : implement check to see if sections defined in compatibility file are the correct size
-- TODO : check that non of the compatibility blocks are secrect
import ParserUtil
import Monad(liftM)
import List(sortBy)

data CompatibilityBlock = CB {
  name :: String,
  start :: Int,
  blockLength :: Int
}
instance Show CompatibilityBlock where
  show b = "Block " ++ name b ++ " {\n"
    ++ "Compatibility " ++ show (start b) ++ ", " ++ show (blockLength b)
    ++ "\n}"
nvramSectionOffset = 188

baseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"
data NvramCompatibilityConfig = MkCompConfig [CompatibilityBlock] deriving (Show)

test_main = do 
  compatConfig <- readFile "NvmCompatibility.pgm"
  case parseCompatibilityConfig compatConfig of 
    Left err -> do  putStrLn "Error parsing input:"
                    print err
    Right (MkCompConfig bs) -> writeFile "NvmCompatibility2.pgm" $ layoutRepresentation bs

p_config :: Parser NvramCompatibilityConfig
p_config = do
  string "NvmConfig BDC {" <* eol
  compatiblityBlocks <- many1 p_compatibilityBlock
  char '}'
  return $ MkCompConfig compatiblityBlocks

p_compatibilityBlock :: Parser CompatibilityBlock
p_compatibilityBlock = do
  name <- spaces *> string "Block " *> many1 (oneOf baseChars) <* spaces <* char '{' <* eol
  start <- read `liftM` (spaces *> string "Compatibility " *> many1 digit <* char ',' <* spaces)
  len <- read `liftM` many1 digit
  spaces *> char '}' *> eol <?> "end of block"
  return $ CB name start len

parseCompatibilityConfig :: String -> Either ParseError NvramCompatibilityConfig
parseCompatibilityConfig input = parse p_config "(unknown)" input

processConfig :: NvramCompatibilityConfig -> String
processConfig (MkCompConfig blocks) = 
  let bs = map show (findBestLayout blocks) in
    unlines bs

findBestLayout :: [CompatibilityBlock] -> [CompatibilityBlock]
findBestLayout bs =
  let sorted = sortBy (\a b -> compare (blockLength a) (blockLength b)) bs in
    reverse $ snd $ foldl (\(off,xs) (CB n _ len) -> (off+len,(CB n off len):xs)) (nvramSectionOffset,[]) sorted

layoutRepresentation :: [CompatibilityBlock] -> String
layoutRepresentation bs = "NvmConfig BDC {\n\n" ++ blocks ++ "}"
  where blocks = snd $ foldl renderBlock (0,[]) bs

renderBlock (offset,output) b = (offset + blockLength b,output ++ "Block " ++ name b ++ " {\n"
          ++ "Compatibility " ++ show offset ++ ", " ++ show (blockLength b) ++ "\n"
          ++ "}\n")
