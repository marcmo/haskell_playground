import Data.List.Split (splitOn)
import Data.List (intercalate)

import Control.Error (readMay)

import Control.Monad(when)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)


data SchemeExpr
  = SInt    Int
  | SFloat  Float
  | SSymbol String
  | SList   [SchemeExpr]
  deriving (Eq, Show)

data SyntaxError = SyntaxError String
  deriving Show

type Token = String



parse :: String -> Either SyntaxError SchemeExpr
parse s = readFrom (tokenize s)

(|>) :: a -> (a -> b) -> b
x |> f = f x

tokenize :: String -> [Token]
tokenize s = s |> replace "(" " ( "
               |> replace ")" " ) "
               |> words

replace :: String -> String -> String -> String
replace old new s = s |> splitOn old
                      |> intercalate new


tryReadInto :: Read a => (a -> b) -> Either String b -> Either String b
tryReadInto _ (Right x) = Right x
tryReadInto f (Left s) = case readMay s of
  Just x -> Right (f x)
  Nothing -> Left s


orElse :: a -> Either err a -> a
orElse a (Left _) = a
orElse _ (Right a) = a


atom :: Token -> SchemeExpr
atom s = Left s |> tryReadInto SInt
                |> tryReadInto SFloat
                |> orElse (SSymbol s)


raise = lift . Left

whileM :: Monad m => m Bool -> m () -> m ()
whileM mb m = do
  b <- mb
  when b (m >> whileM mb m)

readFrom :: [Token] -> Either SyntaxError SchemeExpr
readFrom = evalStateT readFrom'

readFrom' :: StateT [Token] (Either SyntaxError) SchemeExpr
readFrom' = do
  tokens <- get
  case tokens of
    [] -> raise (SyntaxError "unexpected EOF while reading")
    (token:tokens') -> do
      put tokens'
      case token of
        "(" -> (SList . reverse) `fmap` execStateT readWithList []
        ")" -> raise (SyntaxError "unexpected close paren")
        _   -> return (atom token)

readWithList :: StateT [SchemeExpr] (StateT [Token] (Either SyntaxError)) ()
readWithList = do
  whileM ((\toks -> head toks /= ")") `fmap` lift get) $ do
    innerExpr <- lift readFrom'
    modify (innerExpr:)
  lift $ modify (drop 1)
