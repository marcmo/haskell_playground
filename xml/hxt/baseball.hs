{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
import Text.XML.HXT.Core

-- This example demonstrates a more complex XML parse,
-- involving multiple levels, attributes, inner lists,
-- and dealing with optional data.

-- Example data drawn from:
-- http://www.ibiblio.org/xml/books/bible/examples/05/5-1.xml
-- save as: simple2.xml

data Team = Team
  { teamName, division, league, city :: String,
    players :: [Player] }
  deriving (Show, Eq)

data Player = Player
  { firstName, lastName, position :: String,
   
    atBats, hits :: Maybe Int,
    era          :: Maybe Float }
  deriving (Show, Eq)

parseXML file = readDocument [(a_validate,v_0)] file

atTag tag = deep (isElem >>> hasName tag)
-- 
-- -- Incremental development of the getTeams function:
-- 
-- -- First, list the teams.
-- -- Try it out in GHCi:
-- -- Main> runX (parseXML "simple2.xml" >>> getTeams1)
-- 
-- getTeams1 = atTag "LEAGUE" >>>
--   proc l -> do
--     leagName <- getAttrValue "NAME" -< l
--     divi     <- atTag "DIVISION"    -< l
--     diviName <- getAttrValue "NAME" -< divi
--     team     <- atTag "TEAM"        -< divi
--     teamName <- getAttrValue "NAME" -< team
--     returnA -< (leagName, diviName, teamName)
-- 
-- -- getTeams2 also lists the players.
-- -- But there is a catch; now teams without players
-- -- are being left out.  (This behavior is familiar to
-- -- users of the List monad)
-- 
-- getTeams2 = atTag "LEAGUE" >>>
--   proc l -> do
--     leagName <- getAttrValue "NAME"       -< l
--     divi     <- atTag "DIVISION"          -< l
--     diviName <- getAttrValue "NAME"       -< divi
--     team     <- atTag "TEAM"              -< divi
--     teamName <- getAttrValue "NAME"       -< team
--     player   <- atTag "PLAYER"            -< team
--     fName    <- getAttrValue "GIVEN_NAME" -< player
--     lName    <- getAttrValue "SURNAME"    -< player
--     returnA -< (leagName, diviName, teamName, fName, lName)
-- 
-- -- What we really want is to capture the players in a list
-- -- at this level; and if there are no players then the
-- -- empty list will suffice.  listA is used for this purpose.
-- 
-- getPlayer1 = atTag "PLAYER" >>>
--   proc p -> do
--     fName    <- getAttrValue "GIVEN_NAME" -< p
--     lName    <- getAttrValue "SURNAME"    -< p
--     returnA -< (fName, lName)
--    
-- getTeams3 = atTag "LEAGUE" >>>
--   proc l -> do
--     leagName <- getAttrValue "NAME" -< l
--     divi     <- atTag "DIVISION"    -< l
--     diviName <- getAttrValue "NAME" -< divi
--     team     <- atTag "TEAM"        -< divi
--     teamName <- getAttrValue "NAME" -< team
--     players  <- listA getPlayer1    -< team
--     returnA -< (leagName, diviName, teamName, players)
-- 
-- Try capturing some statistics about the players

significant = not . all (`elem` " \n\r\t")

-- Use our definition of "significant" strings to
-- capture the value; or else nothing.

getStat attr =
  (getAttrValue attr >>> isA significant >>> arr Just)
    `orElse` (constA Nothing)

getPlayer2 = atTag "PLAYER" >>>
  proc p -> do
    fName    <- getAttrValue "GIVEN_NAME" -< p
    lName    <- getAttrValue "SURNAME"    -< p
    position <- getAttrValue "POSITION"   -< p
    hits     <- getStat "HITS"            -< p
    atBats   <- getStat "AT_BATS"         -< p
    era      <- getStat "ERA"             -< p
    returnA -< Player
      { firstName = fName,
        lastName  = lName,
        position  = position,
        hits      = read `fmap` hits,
        atBats    = read `fmap` atBats,
        era       = read `fmap` era }
 
getTeams4 = atTag "LEAGUE" >>>
  proc l -> do
    leagName <- getAttrValue "NAME" -< l
    divi     <- atTag "DIVISION"    -< l
    diviName <- getAttrValue "NAME" -< divi
    team     <- atTag "TEAM"        -< divi
    teamName <- getAttrValue "NAME" -< team
    city     <- getAttrValue "CITY" -< team
    players  <- listA getPlayer2    -< team
    returnA -< Team
      { league   = leagName,
        division = diviName,
        teamName = teamName,
        city     = city,
        players  = players }

-- Our final choices

getPlayer = getPlayer2
getTeams  = getTeams4

main = do
  teams <- runX (parseXML "baseball.xml" >>> getTeams)
  print teams
