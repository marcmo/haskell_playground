doGuessing num = do
  putStrLn "Enter your guess:"
  guess <- getLine
  case compare (read guess) num of
    LT -> do putStrLn "Too low!"
             doGuessing num
    GT -> do putStrLn "Too high!"
             doGuessing num
    EQ -> putStrLn "You Win!"

askName = do
  putStrLn "your name:"
  name <- getLine
  case name of
    "Simon" -> putStrLn "hi simon"
    _       -> putStrLn "don't know you"
--  if name `elem` ["Simon","Oli"]
--    then do putStrLn "good"
--    else do putStrLn "bad"
--Simon, John or Phil
