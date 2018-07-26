{-# LANGUAGE NoMonomorphismRestriction #-}
module Nvram where
  
import ParserUtil
import Control.Monad(liftM)
import Data.Maybe(isJust)

data BlockType = Native | DataSet | Redundant deriving (Show)
data CheckSum = Crc16 | Crc32 deriving (Show)
data Block = Block {
  bName :: String,
  id :: Int,
  blockType :: BlockType,
  size :: Int,
  checksum :: Maybe CheckSum,
  datasetSize :: Maybe Int,
  pim :: Maybe String,
  secret :: Bool,
  defaultData :: Maybe [String]
}
instance Show Block where
	show b = ""
		
baseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"
data Swc = Swc String [Block] deriving (Show)
data NvramConfig = NvramConfig [Swc] deriving (Show)

p_config :: Parser NvramConfig
p_config = do
  string "NvmConfig BDC {"
  eol
  swcs <- many1 p_swc
  char '}'
  return $ NvramConfig swcs

p_swc :: Parser Swc
p_swc = do
  name <- spaces *> string "SwcName" *> spaces *> many1 letter <* eol <?> "block start"
  blocks <- many1 p_block
  return $ Swc name blocks

p_type :: Parser BlockType
p_type = DataSet <$ string "dataset" 
     <|> Native <$ string "native"
     <|> Redundant <$ string "redundant"
p_checksum :: Parser CheckSum
p_checksum = Crc16 <$ (try $ string "crc16")
         <|> Crc32 <$ string "crc32"
p_secret :: Parser Bool
p_secret = True <$ string"true"
       <|> False <$ string "false"
p_defaultData :: Parser [String]
p_defaultData = spaces *> string "DefaultData " *> (between (char '\"') (char '\"') ((many1 digit) `sepBy` (many1 space <|> many1 (char '\t')))) <* eol

p_block :: Parser Block
p_block = do
  name <- spaces *> string "Block " *> many1 (oneOf baseChars) <* spaces <* char '{' <* eol
  id <- spaces *> string "ID " *> many1 digit <* eol
  blockType <- spaces *> string "Type " *> p_type <* eol
  size <- p_field "Size" (many1 digit)
  defaultData <- optionMaybe $ try p_defaultData
  secret <- p_field "Secret " p_secret
  checksum <- optionalField (p_field "Checksum" p_checksum)
  datasetSize <- optionalField (p_field "DatasetSize" (many1 digit))
  priority <- optionalField (p_field "Priority" (choice [string "high",string "middle",string "low"]))
  useRtePim <- optionalField (p_field "UseRtePim" (many1 (oneOf (' ':baseChars))))
  notification <- optionalField (p_field "NotificationPortName" (many1 (oneOf baseChars)))
  spaces *> char '}' *> eol <?> "end of block"
  return $ Block name (read id) blockType (read size) checksum (read <$> datasetSize) useRtePim secret defaultData

p_field :: String -> GenParser Char () a -> GenParser Char () a
p_field s p = spaces *> string s *> spaces *> p <* eol <?> s
optionalField p =  optionMaybe $ try p
line :: Parser [Int]
line = (many1 word) <* optional eol
word :: Parser Int
word = liftM read $ count 1 digit

parseInput :: String -> Either ParseError NvramConfig
parseInput input = parse p_config "(unknown)" input

process :: NvramConfig -> String
process (NvramConfig swcs) = concat $ map showSwcInfo swcs

-- where showBlockInfo (bName id blockType :: BlockType, size :: Int, checksum :: Maybe CheckSum, datasetSize :: Maybe Int, pim :: Maybe String, secret :: Bool, defaultData :: Maybe [String])
blockInfoList (Swc n blocks) = map showBlockInfo blocks
	where showBlockInfo x = show x
	-- where showBlockInfo (n id t s crc setSize pim sec defaultD) =
	

swcInfo (Swc n blocks) =
  ["total size: " ++ (show $ foldr sumSize 0 blocks),
  "number of blocks with checksum: " ++ (show $ length $ filter (isJust . checksum) blocks),
  blockInformation pimBlocks "pim blocks",
  blockInformation pimBlocksInternal " --- internal pim blocks",
  blockInformation pimBlocksExternal " --- external pim blocks",
  blockInformation internalBlocks "internal flash blocks",
  blockInformation externalBlocks "external eeprom blocks",
  blockInformation externalCrcBlocks "external crc eeprom blocks",
  blockInformation crcNoDefault "crc blocks without a default value"]
    where sumSize b s = s + (size b)
          internalBlocks = filter secret blocks
          externalBlocks = filter (not . secret) blocks
          externalCrcBlocks = filter (isJust . checksum) externalBlocks
          pimBlocks = filter (isJust . pim) blocks
          pimBlocksInternal = filter secret pimBlocks
          pimBlocksExternal = filter (not . secret) pimBlocks
          crcBlocks = filter (isJust . checksum) blocks
          crcNoDefault = filter (not . (isJust . defaultData)) crcBlocks

showSwcInfo swc =
  unlines $ zipWith (++) (map (\x->show x ++ " : ") [1..]) (swcInfo swc)
 
blockInformation :: [Block] -> String -> String
blockInformation blocks s =
  s ++ ": " ++ (show $ length blocks) ++ '(':(show $ foldr sumSize 0 blocks) ++ " bytes)" ++ (unlines $ map bName blocks)
     where sumSize b = (+) (size b)
    
printInfo ::  IO ()
printInfo = do
    (Right res) <- parseFromFile p_config "/home/omueller/dev/git/BDC/configuration/model/nvram/NvmConfigBDC.pgm" 
    putStrLn $ process res
    
