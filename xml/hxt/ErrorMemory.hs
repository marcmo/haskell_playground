{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module Script.ErrorMemory
where

import Text.XML.HXT.Arrow
import Data.Maybe(isJust,fromJust,catMaybes)
import System.FilePath.FindCompat

findAllPlaces :: IO ([[B.ByteString]])
findAllPlaces = do
  xs <- modelFiles
  let ys = map dtcName xs :: [IO (Maybe [B.ByteString])]
  zs <- sequence ys :: IO [Maybe [B.ByteString]]
  let res = catMaybes zs
  return res

modelFiles :: IO [FilePath]
modelFiles  = find
        (depth <=? 10) 
        (fileType ==? RegularFile &&? extension ==? ".odx-d")
        "/home/omueller/dev/git/FEM/parameters_and_diagnosis/fem/diagnosis/container/F020_DKT_FEM__LR01__BODY_003_000_044/odx/src/diaglayer"

parseXML file = readDocument [(a_validate,v_0)] file

atTag tag = deep (isElem >>> hasName tag)
text_ = getChildren >>> getText

processor :: FilePath -> IOSArrow XmlTree String
processor filename =
    readDocument [(a_validate,"0")] filename >>>
    getChildren >>>
    -- isElem >>> hasName "SHORT-NAME" >>>
    atTag "DTC" >>>
    getChildren >>> isElem >>> hasName "SHORT-NAME" >>>
    getChildren >>>
    getText
play = do { results <- runX (processor xmlPath) ; print results }

getDtcs = atTag "DTC" >>>
  proc d -> do
    troubleCode <- text_ <<< hasName "TROUBLE-CODE" <<< isElem <<< getChildren -< d
    displayTroubleCode <- text_ <<< hasName "DISPLAY-TROUBLE-CODE" <<< isElem <<< getChildren -< d
    returnA -< Dtc troubleCode displayTroubleCode

data Dtc = Dtc {
  getDtcCode :: String,
  getDtcText :: String
} deriving (Show,Eq)   
getDtc = atTag "DTC" >>>
  proc p -> do
    id     <- getAttrValue "ID"    -< p
    returnA -< id
    -- name    <- text_ <<< atTag "SHORT-NAME" -< p
    -- display <- text_ <<< atTag "DISPLAY-TROUBLE-CODE"   -< p
    -- returnA -< Dtc
    --   { getDtcName = name,
    --     getDtcText = display}

-- xmlPath = "FEM_LCE2.odx-d"
xmlPath = "/home/omueller/dev/git/FEM/parameters_and_diagnosis/fem/diagnosis/container/F020_DKT_FEM__LR01__BODY_003_000_044/odx/src/diaglayer/FEM_LCE.odx-d"
test = do
  dtcFiles <- modelFiles
  dtcs <- mapM (\x->runX (parseXML x >>> getDtcs)) dtcFiles
  let allDtcs = concat dtcs :: [Dtc]
  -- dtcs <- runX (parseXML xmlPath >>> getDtcs)
  print $ length allDtcs
  return dtcs
