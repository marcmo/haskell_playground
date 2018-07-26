import FemSparklines
import Data.Time.Format
import Data.Time.Clock
import Data.Time.LocalTime
import System.Locale
import Data.Time.Calendar
-- import Test.Framework (defaultMain, testGroup)
-- import Test.Framework.Providers.HUnit
-- import Test.Framework.Providers.QuickCheck2 (testProperty)
-- 
import Test.QuickCheck
import Test.HUnit

foo :: String -> String -> Maybe Day
foo format = parseTime defaultTimeLocale format

foo2 :: String -> String -> Maybe TimeOfDay
foo2 format = parseTime defaultTimeLocale format

getUtcTime :: String -> Maybe UTCTime
getUtcTime = parseTime defaultTimeLocale formatGit

-- testWithin24hours 

formatGit = "%a, %e %b %Y %H:%M:%S %Z"
dateSampleString = "Wed, 10 Mar 2010 14:21:19 +0000"

parseGit = parseTime defaultTimeLocale formatGit :: String -> Maybe Day

next x = addGregorianMonthsRollOver x

-- main = defaultMain tests
main = do
  mapM_ runTestTT [test24h,test24h2,test24h3]

test24h = TestCase $ assertBool "24 h" $ betweenHours dateToday earlierToday
test24h2 = TestCase $ assertBool "not in 24 h" $ not $ betweenHours dateToday earlierYesterday
test24h3 = TestCase $ assertBool "exactely 24 h" $ betweenHours dateToday sameTimeYesterday
testLastDays = TestCase $ assertEqual "last 10 days" 10 $ length $ lastDays dateToday 10

-- tests = [
--         testGroup "crossing Group" [
--                 -- testProperty "next" prop_next,
--                  testCase "within 24 hours" test_24hours
--             ]
--     ]
(Just dateToday) = getUtcTime "Wed, 10 Mar 2010 14:21:19 +0000"
(Just earlierToday) = getUtcTime "Wed, 10 Mar 2010 04:21:19 +0000"
(Just earlierYesterday) = getUtcTime "Wed, 9 Mar 2010 04:21:19 +0000"
(Just sameTimeYesterday) = getUtcTime "Wed, 9 Mar 2010 14:21:19 +0000"
-- test_24hours = betweenHours dateToday dateToday @?= True
