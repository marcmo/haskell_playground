{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

import           Blaze.ByteString.Builder                 (Builder,
                                                           fromByteString)
import           Blaze.ByteString.Builder.Char8           (fromChar, fromShow)
import           Blaze.ByteString.Builder.Internal.Buffer (allocBuffer)
import           Control.Applicative                      (pure, (<$>), (<*),
                                                           (<|>))
import           Control.Monad                            (void)
import           Data.Attoparsec.ByteString.Char8         (char, decimal,
                                                           endOfInput,
                                                           endOfLine, takeTill,
                                                           takeWhile)
import           Data.ByteString.Char8                    (ByteString)
import qualified Data.HashTable.IO                        as H
import           Data.Maybe                               (fromMaybe)
import           Data.Monoid                              (mconcat)
import           Data.Vector                              (Vector)
import qualified Data.Vector                              as V
import           Prelude                                  hiding (takeWhile)
import           System.IO.Streams                        (InputStream,
                                                           OutputStream)
import qualified System.IO.Streams                        as Streams
import qualified System.IO.Streams.Attoparsec             as Streams


data OutputRecord = OutputRecord {
      _category :: !ByteString
    , _index    :: {-# UNPACK #-} !Int
    , _max      :: {-# UNPACK #-} !Int
    }

data HostRecord = HostRecord {
      _host      :: !ByteString
    , _hostindex :: {-# UNPACK #-} !Int
    }

type Url = ByteString

type HT = H.CuckooHashTable Url ([OutputRecord] -> [OutputRecord])


------------------------------------------------------------------------------
-- Input has format:
-- /category/foo
-- 0. host
-- 1. host
-- /category/next
-- ...
--
-- Given a stream, produce an InputStream that yields hosts from one of these
-- blocks until the next category is reached.
hostsStream :: InputStream ByteString -> IO (InputStream HostRecord)
hostsStream = Streams.parserToInputStream ((Just <$> parser) <|> pure Nothing)
  where
    parser = do
        idx <- decimal
        void $ char '.'
        void $ takeWhile (== ' ')
        host <- takeTill (== '\n')
        void $ endOfLine
        return $! HostRecord host idx


------------------------------------------------------------------------------
getCategory :: InputStream ByteString -> IO (Maybe ByteString)
getCategory = Streams.parseFromStream p
  where
    p = (endOfInput >> pure Nothing) <|>
        (Just <$> takeWhile (/= '\n') <* endOfLine)


------------------------------------------------------------------------------
eatCategory :: InputStream ByteString
            -> IO (Maybe (ByteString, Vector HostRecord))
eatCategory input = getCategory input >>=
                    maybe (return Nothing) ((Just <$>) .  proc)
  where
    proc :: ByteString -> IO (ByteString, Vector HostRecord)
    proc category = do
        hosts <- hostsStream input >>= Streams.toVector
        return (category, hosts)


------------------------------------------------------------------------------
procCategory :: InputStream ByteString
             -> HT
             -> IO Bool
procCategory input ht = eatCategory input >>= maybe (return False) go
  where
    go (category, records) = V.mapM_ add records >> return True
      where
        add (HostRecord host idx) = do
            l <- fromMaybe id <$> H.lookup ht host
            let !r = OutputRecord category idx len
            H.insert ht host (l . (r :))

        len = V.length records


------------------------------------------------------------------------------
readInput :: InputStream ByteString -> HT -> IO ()
readInput input ht = go
  where
    go = do
        b <- procCategory input ht
        if b then go else return ()


------------------------------------------------------------------------------
writeOutput :: OutputStream Builder -> HT -> IO ()
writeOutput output ht = H.mapM_ writeOne ht >> Streams.write Nothing output
  where
    writeOne (host, records) = flip Streams.write output $! Just $!
                               mconcat $ [ fromByteString host,
                                           fromChar '\n'
                                         ] ++ (map record $ records [])
    record (OutputRecord category idx mx) =
        mconcat [ fromChar '('
                , fromShow $! (idx + 1)
                , fromChar '/'
                , fromShow mx
                , fromByteString ") "
                , fromByteString category
                , fromChar '\n'
                ]


------------------------------------------------------------------------------
main :: IO ()
main = do
    ht  <- H.newSized 13000000
    out <- Streams.unsafeBuilderStream (allocBuffer 65535) Streams.stdout
    readInput Streams.stdin ht
    writeOutput out ht

