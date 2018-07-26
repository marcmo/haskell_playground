{-# LANGUAGE OverloadedStrings #-}
import System.Environment(getArgs)
import Control.Monad(liftM,forM_,forM,(>=>))
import Data.List(foldl',sortBy)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.ByteString.Internal(c2w)
import qualified Data.Map as M
import Control.Exception.Base
import System.Posix.Files
import System.IO
import            Control.Concurrent
import Data.Int
import Data.Function
import Data.Maybe(fromJust)
import Debug.Trace
-- import Text.Regex.Posix((=~))

-- main2 = do
--     args <- getArgs
--     forM_ args $ \name -> do
--         m <- (foldl' count M.empty . BC.lines) `fmap` LB.readFile name
--         mapM_ print ((take 10 . sortBy (flip compare `on` snd) . M.toList) m)
--   where on f g x y = g x `f` (g y)
--         count m line = case line =~ "\"GET /en/([^ ]+\\.html)" of
--                          ((_:g:_):_) -> M.insertWith' (+) g 1 m
--                          _ -> m :: M.Map LB.ByteString Int

chunkedLineBoundaries :: Int -> FilePath -> IO [(Int64, Int64)]
chunkedLineBoundaries numChunks path = do
    totalSize <- (fromIntegral . fileSize) `fmap` getFileStatus path
    let chunkSize = totalSize `div` fromIntegral numChunks
    bracket (openFile path ReadMode) hClose $ \h ->
      flip fix 0 $ \findOffsets offset -> do
        let newOffset = offset + chunkSize
        hSeek h AbsoluteSeek (fromIntegral newOffset)
        flip fix newOffset $ \loop off -> do
          eof <- hIsEOF h
          if eof
            then return [(offset, totalSize - offset)]
            else do
              bytes <- LB.hGet h 4096
              case LB.elemIndex (c2w '\n') bytes of
                Just n -> do
                  offsets <- findOffsets (off + n + 1)
                  return ((offset, fst (head offsets) - offset):offsets)
                Nothing -> loop (off + LB.length bytes)
withChunks :: Int -> (LB.ByteString -> a) -> FilePath -> IO [a]
withChunks numThreads f path = do
  offsets <- chunkedLineBoundaries numThreads path
  ch <- newChan
  forM_ offsets $ \(offset, count) -> forkIO $
    handle (\e-> writeChan ch $ Left (e :: SomeException)) $
      bracket (openFile path ReadMode) hClose $ \h -> do
        hSeek h AbsoluteSeek (fromIntegral offset)
        ret <- (f . LB.take count) `fmap` LB.hGetContents h
        ret `seq` writeChan ch (Right ret)
  forM offsets (const (readChan ch >>= either throwIO return))
-- reCountLines :: LB.ByteString -> M.Map LB.ByteString Int
-- reCountLines = foldl' count M.empty . BC.lines
--     where count m line = case line =~ "\"GET /en/([^ ]+\\.html)" of
--                            ((_:g:_):_) -> M.insertWith' (+) g 1 m
--                            _ -> m
countLines ::  BC.ByteString -> M.Map BC.ByteString Int
countLines = fastCountLines
-- countLines = reCountLines
sequential ::  FilePath -> IO (M.Map BC.ByteString Int)
sequential = liftM countLines . LB.readFile
-- parallel = fmap (M.unionsWith (+) . map snd) . withChunks 2 countLines
kind = sequential
-- kind = parallel
main = do
    [p] <- getArgs
    kind p >>= \m -> do
      print m
      mapM_ print ((take 10 . sortBy (flip compare `on` snd) . M.toList) m)
  where on f g x y = g x `f` g y
fastCountLines :: LB.ByteString -> M.Map LB.ByteString Int
fastCountLines f = (trace $ "fastCountLines " ++ show (length $ BC.lines f)) (foldl' count M.empty . BC.lines) f
  where count m line =
          let quote = LB.drop (fromJust (LB.elemIndex (c2w '\"') line)) line
          in if "\"GET /on" `LB.isPrefixOf` quote
             then let pfx = LB.drop 9 quote
                      uri = LB.take (fromJust (LB.elemIndex (c2w ' ') pfx)) pfx
                  in if ".html" `LB.isSuffixOf` uri
                     then M.insertWith' (+) uri 1 m
                     else (trace $ "------------> " ++ show pfx ++ "---------> uri:" ++ show uri) m
             else (trace $ "was not prefix of quote: " ++ show quote) m
