import Text.XML.HXT.Arrow

play arg =
    do { results <- runX (processor arg)
       ; print results
       }

processor :: FilePath -> IOSArrow XmlTree String
processor filename =
    readDocument [(a_validate,"0")] filename >>>
    getChildren >>>
    isElem >>> hasName "html" >>>
    getChildren >>>
    isElem >>> hasName "head" >>>
    getChildren >>>
    isElem >>> hasName "title" >>>
    getChildren >>>
    getText

