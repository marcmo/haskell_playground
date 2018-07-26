main = do putStr "You are? "
          name <- getLine
          print name

foo :: Int -> Int
foo x = (x + 3)