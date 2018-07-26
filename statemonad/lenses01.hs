{-# LANGUAGE TemplateHaskell #-}
import Data.Lens.Lazy
import Data.Lens.Template ( makeLenses )

data Person = Person {
  _fullName :: String,
  _familiarName :: String,
  _surName  :: String
  } deriving (Show)
             
$( makeLenses [''Person] )
henry = Person "Henry Herman Laxen" "Henry" "Laxen"

main = do
  print $ fullName ^$ henry
  print $ henry ^. fullName 

