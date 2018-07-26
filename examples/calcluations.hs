  
main = interact $ calcAllLines subLine
calcAllLines f x = unlines $ map (show . f) (lines x)

addLine :: String -> Int
addLine x = let xs = words x in
  (read $ head xs) + (read $ head $ tail xs)

subLine :: String -> Int
subLine x = let xs = words x in
  (read $ head xs) - (read $ head $ tail xs)
