{-#LANGUAGE MonadComprehensions#-}
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy.Builder as B
import Data.Monoid
import Data.Char
import Data.Maybe

process contents  = mconcat [process_line r <> B.charUtf8 '\n' | r <-  B.split '\n' contents]
process_line line = mconcat [capitalize r   <> B.charUtf8 ' '  | r <- B.split ' ' line]
capitalize word   = fromMaybe mempty
                 [B.charUtf8 (toUpper a) <> B.lazyByteString bs | (a,bs) <- B.uncons word]

main = do contents <- B.readFile  "lorem.txt"
          B.putStrLn $  B.toLazyByteString $ process contents
