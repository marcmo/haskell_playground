{-# LANGUAGE ScopedTypeVariables #-}

-- Takes 18.22s wall clock to complete over 1gb on m modern Intel i7 w/ SSD.

module Cap2 where
import Data.Conduit
import Data.Conduit.Binary
import Data.Char8
import Data.Foldable (fold, foldMap, toList)
import Blaze.ByteString.Builder.Char8  as Builder
import Blaze.ByteString.Builder        as Builder
import qualified Data.ByteString.Char8 as B


capitalizeC :: (Monad m) => Conduit B.ByteString m B.ByteString
capitalizeC = loopChunks ' '
  where loopChunks contextChar = await >>= maybe (return ()) (loopBytes contextChar)
        loopBytes (lastChar :: Char) (moar :: B.ByteString) = do
          let scrubber = \last curr -> if (isSpace last) then toUpper curr else curr
          yield $ B.scanl scrubber lastChar moar
          loopChunks $ B.last moar

main :: IO ()
main = runResourceT $ sourceFile "input.txt" $= capitalizeC $$ sinkFile "output.txt"

