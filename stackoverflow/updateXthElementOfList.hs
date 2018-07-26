module Grid where

data State = On | Off deriving (Eq, Show)

next :: State -> State
next On = Off
next Off = On

type Row = [State]


updateRow :: Row -> Int -> Row
updateRow  rs n = walk n rs []
  where walk 0 (x:xs) res = (reverse $ (next x):res) ++ xs
        walk m (x:xs) res = walk (m-1) xs (x:res)

updateRow2 :: Row -> Int -> Row
updateRow2 rs n = start ++ (next x):end
  where start = take n rs
        end = drop (n+1) rs
        x = rs!!n
