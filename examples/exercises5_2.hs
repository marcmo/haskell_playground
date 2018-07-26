module Main
    where
      
import IO
main = do
  hSetBuffering stdin LineBuffering
  doLoop

doLoop = do
  putStrLn "Do you want to [read] a file, [write] a file or [quit]?"
  command <- getLine
  case command of
    "quit" -> return()
    "read" -> do doReadFile
                 doLoop
    _      -> doLoop

doReadFile = do
  putStrLn "Enter a file name to read" 
  filename <- getLine
  contents <- readFile filename
  putStrLn contents

doWriteFile = do
  putStrLn "Enter a file name to write"
  filename <- getLine
  putStrLn "Enter text (dot on a line by itself to end)"
  text <- do getText ""
  writeFile filename text

getText text = do
  line <- getLine
  case line of
    _:'.' -> text ++ line
    _     -> text ++ getText (text ++ line)
