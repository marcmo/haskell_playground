{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Binary.Get
import Data.Word
import Data.Bits
import Data.Maybe(fromJust)
import qualified Data.Map as M
import Data.List(sort,foldl')
import System.Environment(getArgs)
import Data.Time.Clock

data Event = Event {task::Int,time::Word32} deriving (Eq)
instance Show Event where show (Event ta ti) = "(task:" ++ show ta ++ "|" ++ show ti ++ ")"
instance Ord Event where
  compare (Event _ timeA) (Event _ timeB) = compare timeA timeB
data Interval = Interval { start :: Int, end :: Int } deriving (Show,Eq)
instance Ord Interval where
  compare a b = compare (start a) (start b)
type TaskMap = M.Map Int [Interval]
 
deserializeInfo ::  Get Event
deserializeInfo = do
  task_and_time <- getWord32be
  let t = (task_and_time `shiftR` 24) .&. 0xFF
  let ticks = task_and_time .&. 0xFFFFFF
  return (Event (fromIntegral t) (calculateTime ticks))

showRow :: TaskMap -> (Int,Int) -> BS.ByteString
showRow m (k,taskId) =
  let vs = fromJust $ M.lookup k m
      (sortedHead:sortedRest) = sort vs in
    BS.concat ["{",
                 BC.concat ["\"label\": \"", taskName taskId,"(", BC.pack $ show k,")","\", \"data\": ["],
                 foldl' addToRow (showInterval sortedHead) sortedRest,
                 "]",
                 "}"]
          where showPair (a,b) = BC.pack $ "[" ++ show a ++ "," ++ show b ++ "]"
                showInterval (Interval s e) = BS.concat [showPair(s,k),",",showPair(e,k)]
                addToRow :: BS.ByteString -> Interval -> BS.ByteString
                addToRow acc i = BS.concat [acc,",null,",showInterval i]

main :: IO ()
main = do
  getArgs >>= print
  inputFile:outFile:cnt:_ <- getArgs
  startTime <- getCurrentTime
  input <- BL.readFile inputFile
  res <- parseInput [] input
  let eventList = tail $ take (read cnt) $ reverse res
  let normalizedEvents = norm eventList
  let m = buildMapping normalizedEvents
  let keyPairs = zip (M.keys m) [5,15..]
  let m2 = M.mapKeys (\x->fromJust $ lookup x keyPairs) m
  let os = map (showRow m2) (zip (M.keys m2) (M.keys m))
  let result = BS.concat ["{\"testname\":\"some other values...\",\"testrows\":[",
                          BS.intercalate "," os,
                          "]}"]
  print $ length normalizedEvents
  parsingEnd <- getCurrentTime
  BC.writeFile outFile result
  endTime <- getCurrentTime
  putStrLn $ "time spent parsing:  " ++ show (diffUTCTime parsingEnd startTime)
  putStrLn $ "overall time spent:  " ++ show (diffUTCTime endTime startTime)
  return()
    where parseInput xs s = do
            let (e,rest,_) = runGetState deserializeInfo s 0
            if BL.length rest > 0
            	then parseInput (e:xs) rest
            	else return xs

norm :: [Event] -> [Event]
norm xs = map (\e->Event (task e) (time e - minTime)) $ go xs xs (maxTime xs) 1
  where maxTime = foldl' (\maxT e->max (time e) maxT) 0
        minTime = foldl' (\minT e->min (time e) minT) 100000000 xs
        go (a:b:bs) fullList maxT cnt
          | b > a = go (b:bs) fullList maxT (cnt+1)
          | otherwise = let listStart = take cnt fullList
                            rest = map (\e->Event (task e) (time e+maxT))(drop cnt fullList) in
              listStart ++ go rest rest maxT 1
        go [Event ta ti] fullList maxT cnt
          | ti >= maxT = fullList
          | otherwise = take cnt fullList ++ [Event ta (ti+maxT)]
        go _ _ _ _ = error "pattern match error"


showEventDiffs :: [Event] -> IO()
showEventDiffs (a@(Event _ eventTime):b@(Event _ time2):bs) = do
    let diff = time2 - eventTime
    putStr (display a) >> putStrLn ("[duration:]" ++ show diff ++ ",[end:]" ++ show time2) >> showEventDiffs (b:bs)
showEventDiffs _ = return ()

buildMapping :: [Event] -> TaskMap
buildMapping xs = let m = M.empty in
  traverse xs m
    where traverse (Event eventTask eventTime:b@(Event _ time2):bs) m = traverse (b:bs) m'
            where m' = M.insertWith (++) eventTask [Interval (fromIntegral eventTime) (fromIntegral time2)] m
          traverse _ m = m

taskName t
  | t < 50 = "interrupt"
  | t == 50 = "fusi"
  | t == 51 = "lin"
  | t == 52 = "app"
  | t == 53 = "dem"
  | t == 54 = "background"
  | t == 55 = "idle"
  | t == 60 = "cyclicCallback"
  | t == 61 = "setEvent"
  | otherwise = "unknown"
display :: Event -> String
display (Event t ttime) = render $ taskName t ++ "(" ++ show t ++ ")"
    where render a = show t ++ ":" ++ show ttime ++ " - " ++ a ++ " " ++ show t

calculateTime ::  (Integral b, Integral a) => a -> b
calculateTime t = truncate $ fromIntegral t / 112


