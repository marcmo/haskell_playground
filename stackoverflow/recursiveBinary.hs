import Control.Monad
import qualified Control.Monad.State as S
import Data.Binary

-- type MyMonadT s = StateT [String] Put s
-- foo :: String -> MyMonad
-- foo = undefined
import System.Directory (doesDirectoryExist,getDirectoryContents)
import System.FilePath ((</>))
import Control.Monad (forM_, when)
import Control.Monad.Trans (lift,liftIO)
import Control.Monad.Writer (WriterT, tell,execWriterT)

type Entries = S.State [String]
type MyStateT m = S.StateT [Int] m
lD :: FilePath -> Entries [Int]
lD p = do
  s <- S.get
  return [length s]

type BinStateT = MyStateT Put
cE2 :: FilePath -> MyStateT Entries ()
cE2 path = do
  contents <- lift . lD $ path
  S.modify $ \s -> (length contents):s

data Ref = Ref {
    refName :: String,
    refRefs :: [Ref]
}

instance Binary Ref where
    put a = put (refName a) >> put (refRefs a)
    get = liftM2 Ref get get

main = do
  let r1 = Ref "r1" []
  let r2 = Ref "r2" [r2]
  let r3 = Ref "r3" [r2]
  print "hello"
  encodeFile "test.bin" r2

