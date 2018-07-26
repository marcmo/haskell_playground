import Data.Tree
import Data.List(intercalate)
import Data.List.Split(splitOn)
import System.Environment (getArgs)

data Atom = Sym String | NInt Int | NDouble Double | Para deriving (Eq, Show)

type ParserStack = (Tree Atom, Tree Atom)

replace :: String -> String -> String -> String
replace old new = intercalate new . splitOn old

tokenize = words . replace "(" " ( " . replace ")" " ) "

atom :: String -> Atom
atom tok =
  case reads tok :: [(Int, String)] of
    [(int, _)] -> NInt int
    _ -> case reads tok :: [(Double, String)] of
      [(dbl, _)] -> NDouble dbl
      _ -> Sym tok

empty = Node $ Sym "dummy"
para = Node Para

parseToken (Node _ stack, Node _ out) "(" =
  (empty $ stack ++ [empty out], empty [])
parseToken (Node _ stack, Node _ out) ")" =
  (empty $ init stack, empty $ subForest (last stack) ++ [para out])
parseToken (stack, Node _ out) tok =
  (stack, empty $ out ++ [Node (atom tok) []])

interpret :: String -> IO ()
interpret contents = do
  let tokens = tokenize contents
      parseStack = foldl parseToken (empty [], empty []) tokens
      schemeTree = head $ subForest $ snd parseStack
  putStrLn $ drawTree $ fmap show schemeTree

main = do
  (file:_) <- getArgs
  contents <- readFile file
  interpret contents
