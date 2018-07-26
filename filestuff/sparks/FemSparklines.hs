module FemSparklines where

import Data.List(genericLength,groupBy,sort)
import Data.Maybe(catMaybes)
import Graphics.Rendering.HSparklines
import System.Directory(getCurrentDirectory)
import System.FilePath((</>))
import Control.Monad
import IO
import System.Locale(defaultTimeLocale)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format

png1 = "fem-monthly.png"
png2 = "fem-daily.png"
png3 = "fem-all.png"
png4 = "fem-today.png"

graph = barSpark { bgColor = rgb 0xEE 0xEE 0xEE }
smoothGraph = smoothSpark { bgColor = rgb 0xEE 0xEE 0xEE }


createSparklines src = do
    let dates = catMaybes . sort . map parse . lines $ src :: [UTCTime]

    let startDate  = head dates
        latestDate = last dates
        allMonth   = findAllMonth startDate latestDate
        last10Days = lastDays latestDate 10
        permonth   = groupBy sameMonth dates
        thismonth  = groupBy sameDay . filter (sameMonth latestDate) $ dates
        monthlies  = map genericLength permonth
        dailies    = map genericLength thismonth
        perhour    = groupBy sameHour . filter (betweenHours latestDate) $ dates
        perhour2 d = groupBy sameHour . filter (betweenHours d) $ dates
        -- perhourLastDays = map perhour (lastDays 7 latestDate)
        hourlies   = map genericLength perhour
        alls       = reverse. reverse $ map genericLength (groupBy (\a b -> sameMonth a b && sameDay a b) dates)
        topMonthly = maximum monthlies
        topAll     = maximum alls
        topDaily   = maximum dailies
        topPerHour = maximum hourlies
        monthsPngNames = map (\m->take 7 (show m) ++ ".png") allMonth

    graph1 <- make (graph { limits = (0,round topMonthly) })  monthlies
    graph2 <- make (graph { limits = (0,round topDaily) }) dailies
    graph3 <- make (smoothGraph { limits = (0,round topAll) }) alls
    graph4 <- make (graph { limits = (0,round topPerHour) }) hourlies
    monthGraphs <- forM allMonth $ \m -> do
                    let currentMonth = groupBy sameDay . filter (sameMonth m) $ dates
                    make (graph { limits = (0,round topAll) }) (map genericLength currentMonth)

    -- saveGraphsAsPng monthGraphs monthsPngNames
    -- savePngFile png1 graph1
    -- savePngFile png2 graph2
    -- savePngFile png3 graph3
    savePngFile png4 graph4
    print perhour
    print hourlies

    where
      formatGit = "%a, %e %b %Y %H:%M:%S %Z"
      parse :: ParseTime t => String -> Maybe t
      parse = parseTime defaultTimeLocale formatGit
      saveGraphsAsPng gs pngNames = mapM_ (uncurry savePngFile) (zip pngNames gs)

getMonth (UTCTime x _) = let (_,m,_) = toGregorian x in m
getYear (UTCTime x _) = let (y,_,_) = toGregorian x in y
getDay (UTCTime x _) = let (_,_,d) = toGregorian x in d

sameMonth a b = getMonth a == getMonth b && getYear a == getYear b
sameDay a b = sameMonth a b && getDay a == getDay b
sameHour a@(UTCTime _ t1) b@(UTCTime _ t2) = sameDay a b && todHour (timeToTimeOfDay t1) == todHour (timeToTimeOfDay t2)
betweenHours a x = diffUTCTime a x <= 24*3600

findAllMonth :: UTCTime -> UTCTime -> [UTCTime]
findAllMonth a b  = collectMonth a b []
  where collectMonth :: UTCTime -> UTCTime -> [UTCTime] -> [UTCTime]
        collectMonth x@(UTCTime d1 t1) y@(UTCTime d2 t2) ms
          | x > y = []
          | sameMonth x y = ms
          | otherwise = collectMonth (UTCTime (addGregorianMonthsRollOver 1 d1) t1) y (ms ++ [x])

lastDays :: UTCTime -> Int -> [UTCTime]
lastDays (UTCTime d t) n = take n $ map (\x->(UTCTime (addDays x d) t)) [0,-1..]
-- lastDays n d = [addUTCTime ((-24) * 3600 * x) d | x <- [0..n]]
          
oneHourLater ::  TimeOfDay -> TimeOfDay
oneHourLater t = timeToTimeOfDay $ timeOfDayToTime t + (secondsToDiffTime 3600) 

