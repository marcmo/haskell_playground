{-# LANGUAGE TemplateHaskell #-}
import Data.Lens.Lazy
    ( (~=), access, (%=), mapLens, (^=), (^.), (^%=), (^%%=), (^$) )
import Data.Lens.Template ( makeLenses )
import Data.Char ( toUpper )
import Data.Map ( Map, fromList )
import Control.Monad.State.Lazy ( Monad(return), State, runState,execState )
data Number = Number {
	_value :: Int
} deriving (Show)

data Person = Person {
  _fullName :: String,
  _familiarName :: String,
  _surName  :: String,
  _address  :: Address } deriving (Show)
             
data Address = Address {
  _which :: String,
  _street :: String,
  _city :: String,
  _state :: String,
  _zip  :: String } deriving (Show)

$( makeLenses [''Person, ''Address, ''Number] )
henry = Person
  "Henry Herman Laxen"
  "Henry"
  "Laxen"
  (Address 
    "home"
    "Via Alta #6"
    "Chapala"
    "Jalisco"
    "45900")
upper :: String -> String
upper = Prelude.map toUpper
test1 = address ^$ henry
test2 = henry ^. address
test3 = street ^$ address ^$ henry
test4 = (henry ^. address) ^. street
-- test4a = henry ^. address ^. street 
test5 = fullName ^= "Henry H. Laxen" $ henry
test6 = fullName ^%= upper   $ henry
test7 = fullName ^%= upper   $ test5
nadine = Person
  "Nadine Callaway Laxen"
  "Nadine"
  "Laxen"
  (address ^$ henry)
nadineAgain :: Person
nadineAgain = (fullName ^= "Nadine Callaway Laxen") .
              (familiarName ^= "Nadine") $ henry
addMister :: String -> Maybe String
addMister x = Just ( "Mr. " ++ x )
mister :: Maybe Person
mister = fullName ^%%= addMister $ henry
type ByFamiliarName = Map String Person
justHenry :: ByFamiliarName
justHenry = fromList [("Henry",henry)]
addNadine :: ByFamiliarName -> ByFamiliarName
addNadine = mapLens "Nadine" ^= Just nadine
removeHenry :: ByFamiliarName -> ByFamiliarName
removeHenry = mapLens "Henry" ^= Nothing    
test8 = addNadine justHenry
test9 = removeHenry (addNadine justHenry)
changeFamiliarWithState :: State Person String
changeFamiliarWithState = do
  x <- access familiarName
  if x == "Nadine" then familiarName ~= "Sweetie"
                  else familiarName %= upper     
  return x
test10 = runState changeFamiliarWithState henry
test11 = runState changeFamiliarWithState nadine
increment :: State Number Int
increment = value %= succ
load :: Int -> State Number Int
load x = value ~= x

test12 = runState (do 
          load 5
          increment) (Number 0)
run ::  State Number a -> Number
run x = execState x (Number 0)
test13 = run (load 5 >> increment >> increment)

