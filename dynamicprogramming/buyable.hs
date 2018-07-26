import Data.Array

buyable n = r!n
    where r = listArray (0,n) (True : map f [1..n])
          f i = i >= 6 && r!(i-6) || i >= 9 && r!(i-9) || i >= 20 && r!(i-20)

buy n = r!n
    where r = listArray (0,n) (Just (0,0,0) : map f [1..n])
          f i = case attempt (i-6)
                of Just(x,y,z) -> Just(x+1,y,z)
                   _ -> case attempt (i-9)
                        of Just(x,y,z) -> Just(x,y+1,z)
                           _ -> case attempt (i-20)
                                of Just(x,y,z) -> Just(x,y,z+1)
                                   _ -> Nothing
          attempt x = if x>=0 then r!x else Nothing
