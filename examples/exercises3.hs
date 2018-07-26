findMax lst = foldr max 0 lst

firstOfSecond:: [(a,a)] -> a
firstOfSecond lst = fst (head (tail lst))

myFib:: Num a => a -> a
myFib 1 = 1
myFib 2 = 1
myFib x = (myFib (x - 2)) + (myFib (x - 1))

mult:: Num a => a -> a -> a
mult x 1 = x
mult x y = x + mult x (y - 1)

askForWords = do                      
  putStrLn "Please enter a word:"     
  word <- getLine                     
  if word == ""                       
  then return []                      
  else do                             
    rest <- askForWords               
    return (word : rest)
                   
fact::Num a => a -> a
fact 1 = 1
fact x = x * fact(x - 1)

facts [] = return()
facts (x:xs) = do
  putStrLn (show x ++ " factorial is " ++
            show(fact x))
  facts xs

askForNumber = do      
  nums <- getNums2    
  putStrLn ("sum: " ++ show (sum nums))
  facts nums

getNums2 = do                                        
  putStrLn "Give me a number (or 0 to stop):"        
  numStr <- getLine                                  
  let num = read numStr                    
  if num == 0                    
  then return []                 
  else do rest <- getNums2       
          return ((num :: Int):rest)

			 
