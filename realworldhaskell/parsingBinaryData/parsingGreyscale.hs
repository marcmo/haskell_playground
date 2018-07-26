import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Lazy
import Data.Char (isSpace)
import Data.Word
import Data.Int

data Greymap = Greymap {
      greyWidth :: Int
    , greyHeight :: Int
    , greyMax :: Int
    , greyData :: L.ByteString
    } deriving (Eq)
instance Show Greymap where
    show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++
                             " " ++ show m

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader h s
    | h `L.isPrefixOf` s = Just (L.dropWhile isSpace (L.drop (L.length h) s))
    | otherwise = Nothing
getNat :: (Num b) => L.ByteString -> Maybe (b, L.ByteString)
getNat s = case L.readInt s of
             Nothing -> Nothing
             Just (i, s') | i <= 0    -> Nothing
                          | otherwise -> Just (fromIntegral i, s')
getBytes :: (Integral a) => a -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes n s = let n' = fromIntegral n
                   ht@(h, t) = L.splitAt n' s
               in if L.length h < n'
                  then Nothing
                  else Just ht
data ParseState = ParseState {
      string :: L.ByteString
    , offset :: Int64
    } deriving (Show)
parseP5_take2 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5_take2 s =
    matchHeader (L.pack "P5") s       >>?
    \s -> skipSpace ((), s)           >>?
    (getNat . snd)                    >>?
    skipSpace                         >>?
    \(width, s) ->   getNat s         >>?
    skipSpace                         >>?
    \(height, s) ->  getNat s         >>?
    \(maxGrey, s) -> getBytes 1 s     >>?
    (getBytes (width * height) . snd) >>?
    \(bitmap, s) -> Just (Greymap width height maxGrey bitmap, s)

skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace (a, s) = Just (a, L.dropWhile isSpace s)
(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v >>? f  = f v
newtype Parse a = Parse {
      runParse :: ParseState -> Either String (a, ParseState)
    }
identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))
parse :: Parse a -> L.ByteString -> Either String a
parse f s = case runParse f (ParseState s 0) of
              Left err -> Left err
              Right (a, _) -> Right a
parseByte :: Parse Char
parseByte =
    getState ==> \st ->
    case L.uncons (string st) of
      Nothing -> bail "no more input"
      Just (c, s) -> let st' = st { string = s, offset = offset st + 1 }
                     in putState st' ==> \_ -> identity c
(==>) :: Parse a -> (a -> Parse b) -> Parse b
x ==> f = Parse (\st -> case runParse x st of
                          Left err -> Left err
                          Right (a, st') -> runParse (f a) st')
getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))
bail :: String -> Parse a
bail err = Parse $ \s -> Left $
           "byte offset " ++ show (offset s) ++ ": " ++ err
