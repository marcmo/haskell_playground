module Main where
import Text.XML.HaXml
import Text.XML.HaXml.Xml2Haskell
import Char(digitToInt)

data CProjectConfig = Subconfig String deriving (Show)
data Cproject = CProject Settings deriving (Show)
data Settings = MkSettings BuildSystem deriving (Show)
data BuildSystem = MkBuildSystem Configuration deriving (Show)
data Configuration = MkConfig FolderInfo deriving (Show)
data FolderInfo = MkFolderInfo ToolChain deriving (Show)
data ToolChain = Option IncludePaths deriving (Show)
type IncludePaths = [String]

instance XmlContent ToolChain where
  fromElem ((CElem e):cs) = (Just (Option ["hi"]),[])
  toElem (Option (x:xs)) = [CString False x]
-- Concise XSLT-like specification of output
main = processXmlWith (hexagrams `o` tag "IChing")
-- main2  = readXml
hexagrams = 
    html [
      hhead [htitle [keep /> tag "title" /> txt] ], 
      hbody [htableBorder [rows `o` children `with` tag "hexagram"] ]
    ]
htableBorder = mkElemAttr "TABLE" [("BORDER",("1"!))]
rows f =
    let
      num = keep /> tag "number" /> txt
      nam = keep /> tag "name" /> txt
      jdg = keep /> tag "judgement" /> txt 
    in
      if (condition (num f) (nam f) (jdg f))
      then hrow [hcol [num], hcol [nam], hcol [jdg]] f
      else []
condition num nam jdg = isPrime (makeInt num)

-- Supporting computations for rows condition
makeInt = stringToInt . unwrap      -- Turn a Content into an Integer
unwrap [(CString b c)] = c          -- Turn a Content into a String
stringToInt = revToInteger.reverse  -- Turn a String into an Integer
    where
    revToInteger = toInteger . revToInt
    revToInt []  = 0
    revToInt (d:ds) = digitToInt d + (10*(revToInt ds))
isPrime = ordSearch (sieve [2..])   -- ordered search of Sieve of Eratosthenes
    where               
    ordSearch (x:xs) n
        | x < n     = ordSearch xs n
        | x == n    = True
        | otherwise = False
    sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x > 0]
