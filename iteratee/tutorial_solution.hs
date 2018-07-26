-- Exercises : given a text file of Ints separated by newlines, write a
-- function which returns the first Int greater than a given k, or
-- Nothing.  Do this once using explicit handle operations (hGetLine) and
-- again using lazy IO (hGetContents.)
import System.IO
import Control.Applicative

solutionA = do
  h <- openFile "ints.txt" ReadMode
  firstGreater 10 h >>= print
  hClose h

firstGreater n h = do
  x <- read <$> hGetLine h
  if x > n then return x else firstGreater n h

solutionB = do
  h <- openFile "ints.txt" ReadMode
  xs <- (map read) <$> lines <$> hGetContents h
  return (loop xs) >>= print
  hClose h
    where loop (x:xs) 
            | x > 10 = x
            | otherwise = loop xs





