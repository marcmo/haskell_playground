import FemSparklines
import IO

main :: IO ()
main = do
    bracket (openFile "commitdate.txt" ReadMode) hClose $ \h ->
       do contents <- hGetContents h
          createSparklines contents
