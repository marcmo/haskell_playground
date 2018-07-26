{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as B
import Data.List (foldl')
import Text.Regex.PCRE.Light (compile, match, Regex)
import Control.Monad (forM)
import Control.Exception (bracket, finally)
import System.IO 
import System.Directory(setCurrentDirectory)
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Control.Applicative
import Numeric(readHex)
-- import HSH.Command

workspace_loc = "/home/omueller/dev/git/ethernet.git"
type IntParser = CharParser() Int


blockRegex :: String -> Regex
blockRegex s = compile (B.pack $ s ++ "\\s*([0-9a-fA-F]){8}") []

freeFlashRegex = blockRegex "__FREE_FLASH_SIZE"
usedFlashRegex = blockRegex "__USED_FLASH_SIZE"
smallBlockFreeRamRegex = blockRegex "__SMALL_BLOCK_FREE_RAM"
bigBlockFreeRamRegex = blockRegex "__BIG_BLOCK_FREE_RAM"
-- freeFlashRegex = compile "__FREE_FLASH_SIZE\\s*([0-9a-fA-F]){8}" []
-- usedFlashRegex = compile "__USED_FLASH_SIZE\\s*([0-9a-fA-F]){8}" []
-- smallBlockFreeRamRegex = compile "__SMALL_BLOCK_FREE_RAM\\s*([0-9a-fA-F]){8}" []
-- bigBlockFreeRamRegex = compile "__BIG_BLOCK_FREE_RAM\\s*([0-9a-fA-F]){8}" []


-- r = compile "^(b+|a){1,2}?bc" []
toRegex s = compile (B.pack s) []
strict  = B.concat . LB.toChunks

getSize :: IntParser -> String -> Int
getSize parser line = case parseFlashSize parser line of
                        Right r -> r
                        _ -> 0

getMapfileLines2 :: [String] -> FilePath -> IO [[String]]
getMapfileLines2 patterns path = do
    bracket (openBinaryFile path ReadMode) hClose $ \h -> do
      content <- LB.hGetContents h
      let res = map (\p -> map B.unpack $ filterLines (blockRegex p) content) patterns 
      print res
      return res


getMapfileLines :: FilePath -> IO ([String],[String],[String],[String])
getMapfileLines path = do
    bracket (openBinaryFile path ReadMode) hClose $ \h -> do
      content <- LB.hGetContents h
      let freeFlashLines = filterLines freeFlashRegex content
      print freeFlashLines -- needs to be done...otherwise this will be evaluated lazily
      let usedFlashLines = filterLines usedFlashRegex content
      print usedFlashLines
      let smallRamLines = filterLines smallBlockFreeRamRegex content
      print smallRamLines
      let bigRamLines = filterLines bigBlockFreeRamRegex content
      print bigRamLines
      return (map B.unpack freeFlashLines,
              map B.unpack usedFlashLines,
              map B.unpack smallRamLines,
              map B.unpack bigRamLines)
  
filterLines :: Regex -> LB.ByteString -> [B.ByteString]
filterLines reg chunk = reverse $ foldl' augment [] (LB.lines chunk)
    where augment accum line = case match reg (strict line) [] of
                                  Just _ -> (strict line):accum
                                  _ -> accum

parseFlashSize :: IntParser -> [Char] -> Either ParseError Int
parseFlashSize p input = parse p "(unknown)" input

lineParser x = (many $ char ' ') *> string x *> hexnum
freeFlashSize = (many $ char ' ') *> string "__FREE_FLASH_SIZE " *> hexnum
usedFlashSize = (many $ char ' ') *> string "__USED_FLASH_SIZE " *> hexnum
freeSmallRamSize = (many $ char ' ') *> string "__SMALL_BLOCK_FREE_RAM " *> hexnum
freeBigRamSize = (many $ char ' ') *> string "__BIG_BLOCK_FREE_RAM " *> hexnum
hexnum = string2hex <$> many1 hexDigit <?> "hexnum"
eol = try (string "\r\n") <|> string "\n" <?> "EOL"
string2hex = fst . head . readHex

test2 = do
  let symbolNames = ["__FREE_FLASH_SIZE","__USED_FLASH_SIZE","__SMALL_BLOCK_FREE_RAM","__BIG_BLOCK_FREE_RAM"]
  filteredLines <- getMapfileLines2 symbolNames $ workspace_loc ++ "/cam_integration_main/Debug_Fado/camera_debug.map"
  let parsers = map lineParser symbolNames
  let parsedSize = map (\getSize freeFlashSize . head) parsers
  -- let freeBigRam = getSize freeBigRamSize $ head bigRamLines
  setCurrentDirectory workspace_loc
  r <- runML "git log -1"
  d <- runML "date"
  bracket (openFile ("/home/omueller/dev/git/ethernet_flashAndRam.txt") AppendMode) hClose $ \h -> do
      mapM_ (hPutStrLn h) d
      mapM_ (hPutStrLn h) r
      hPutStrLn h (replicate 30 '=')
      -- hPutStrLn h $ "free flash: " ++ show freeSize
      -- hPutStrLn h $ "used flash: " ++ show usedSize
      -- hPutStrLn h $ "free ram: " ++ show (freeSmallRam + freeBigRam)
      -- hPutStrLn h $ "used ram: " ++ show (0x10000 + 0x80000 - (freeSmallRam + freeBigRam))
      hPutStrLn h (replicate 60 '-')
      return ()
  return r

runML :: String -> IO [String]
runML c = run c
test = do
  (freeLines,usedLines,smallRamLines,bigRamLines) <- getMapfileLines $ workspace_loc ++ "/cam_integration_main/Debug_Fado/camera_debug.map"
  let freeSize = getSize freeFlashSize $ head freeLines
  let usedSize = getSize usedFlashSize $ head usedLines
  let freeSmallRam = getSize freeSmallRamSize $ head smallRamLines
  let freeBigRam = getSize freeBigRamSize $ head bigRamLines
  setCurrentDirectory workspace_loc
  r <- runML "git log -1"
  d <- runML "date"
  bracket (openFile ("/home/omueller/dev/git/ethernet_flashAndRam.txt") AppendMode) hClose $ \h -> do
      mapM_ (hPutStrLn h) d
      mapM_ (hPutStrLn h) r
      hPutStrLn h (replicate 30 '=')
      hPutStrLn h $ "free flash: " ++ show freeSize
      hPutStrLn h $ "used flash: " ++ show usedSize
      hPutStrLn h $ "free ram: " ++ show (freeSmallRam + freeBigRam)
      hPutStrLn h $ "used ram: " ++ show (0x10000 + 0x80000 - (freeSmallRam + freeBigRam))
      hPutStrLn h (replicate 60 '-')
      return ()
  print freeSize
  print usedSize
  return r


-- calcUsedRam :: Int -> Int
calcUsedRam  = 
  let small_block_free_ram = 0x00008bb8
      big_block_free_ram = 0x0001816c
      small_block_used_ram = 0x00007448
      big_block_bss_size = 0x0001fe94
      big_block_heap_size = 0x00048000
      big_block_used_ram = 0x00067e94 in
    (small_block_free_ram + small_block_used_ram,
     big_block_free_ram + big_block_used_ram)
