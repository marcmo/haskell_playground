import Text.Html

htmlPage = header
           << thetitle 
           << "My Haskell Home Page"
                  +++ body ! [bgcolor "#aaff88"] << theBody

--theBody :: [Html]
theBody =
    table ! [border 0] << tableContents
              +++ br
              +++ p << message

message = "Haskell is a general purpose, purely functional programming language."


tableContents :: HtmlTable
tableContents = (haskell `above` purely) `beside` lambda
    where
      haskell = td ! [align "center"]
                  << font ![size "7",face "Arial Black"] 
                      << "Haskell"
      purely  = td << font ! [size "6"] 
                      << "A Purely Functional Language"
      lambda  = td << image ! [src "lambda.gif"]

main = writeFile "example.htm" (renderHtml htmlPage)
