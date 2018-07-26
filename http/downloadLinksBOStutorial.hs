import Network.HTTP.Enumerator
import Data.ByteString.Lazy.UTF8
import Text.HTML.TagSoup
import Network.URI
import Data.Maybe
import System.Environment

makeFileName :: Int -> FilePath
makeFileName k = "download-" ++ show k ++ ".html"

processPage ::  String -> IO [[Char]]
processPage url = do
  page <- download url
  return (process page)

links :: String -> String -> [String]
links url = catMaybes .
            map (canonicalizeLink url) .
            filter (not . null) .
            map (fromAttrib "href") .
            filter (\t -> fromAttrib "rel" t /= "nofollow") .
            filter (isTagOpenName "a") .
            canonicalizeTags .
            parseTags

canonicalizeLink :: String -> String -> Maybe String
canonicalizeLink referer path = do
  r <- parseURI referer
  p <- parseURIReference path
  n <- p `nonStrictRelativeTo` r
  let u = uriToString id n ""
  return (takeWhile (/= '#') u)
nofollow tag = fromAttrib "rel" tag == "nofollow"

process ::  [Char] -> [[Char]]
process url =
    -- catMaybes $ map (canonicalize url) $
    filter (not . null) .
    map (fromAttrib "href") .
    filter (not . nofollow) .
    filter (isTagOpenName "a") .
    canonicalizeTags .
    parseTags $ url

download :: String -> IO String
download url = do
  page <- simpleHttp url
  return (toString page)

canonicalize :: String -> String -> Maybe String
canonicalize referer path = do
  r <- parseURI referer
  p <- parseURIReference path
  u <- nonStrictRelativeTo p r
  return (uriToString id u "")

main = do
  args <- getArgs
  putStrLn ("So! Your args are " ++ show args)
  page <- download (head args)
  print (process page)


