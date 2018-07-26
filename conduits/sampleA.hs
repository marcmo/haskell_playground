import Data.Conduit
import qualified Data.Conduit.List as CL

source :: Source IO Int -- produces a stream of Ints
source = CL.sourceList [1..4]

sink :: Sink String IO () -- consumes a stream of Strings, no result
sink = CL.mapM_ putStrLn

sink2 :: Sink Int IO () -- consumes a stream of Strings, no result
sink2 = CL.mapM_ print

conduit :: Conduit Int IO String -- converts Ints into Strings
conduit = CL.map show

conduit2 :: Conduit Int IO Int
conduit2 = CL.map (2*)

main = do
    source $$ conduit =$ sink
    source $= conduit2 $$ sink2
        -- alternatively, with the same meaning
