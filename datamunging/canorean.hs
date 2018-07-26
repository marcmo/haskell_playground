import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy.Builder as TB
import Data.Monoid ((<>), mconcat)
import Data.Char
import Data.List

convert = unlines' . map convertLine . T.lines
convertLine = unwords' . map convertWord . T.words
convertWord s = TB.fromString [toUpper (T.head s)] <> TB.fromLazyText (T.tail s)

unlines' = mconcat . intersperse (TB.fromString "\n")
unwords' = mconcat . intersperse (TB.fromString " ")

main = do
    name <- TIO.readFile "lorem.txt"
    TIO.putStr $ TB.toLazyText $ convert name

