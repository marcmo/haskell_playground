module Stack
    where
      
type Stack w = [w]

swap:: Stack w -> Stack w
swap [] = []
swap (x:[]) = x:[]
swap (w1:w2:ws) = w2:w1:ws
 
focusNext:: Stack w -> Stack w
focusNext [] = []
focusNext (w:[]) = w:[]
focusNext (w:ws) = ws ++ [w]

focusPrev:: Stack w -> Stack w
focusPrev = reverse.focusNext.reverse

type TS = Stack Int

prop_focusNP:: TS -> Bool
prop_focusNP s = focusNext(focusPrev s) == s

prop_swap:: TS -> Bool
prop_swap s = swap(swap s) == s

