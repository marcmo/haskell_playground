import Char
import List
import Test.QuickCheck
import Test.QuickCheck.Test
import Text.Printf
 
main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests
 
-- and add this to the tests list
tests  = [("reverse.reverse/id", test prop_reversereverse)]

instance Arbitrary Char where
    arbitrary     = choose ('\0', '\128')
    coarbitrary c = variant (ord c `rem` 4)

-- reversing twice a finite list, is the same as identity
prop_reversereverse s = (reverse . reverse) s == id s
    where _ = s :: [Int]
 
