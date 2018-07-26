import Text.Regex
import Maybe(isJust)

-- test = "foo" =~ "o"
-- | Search for the regexp in the lines.  Return those that match.
grep :: String -> [String] -> [String]
grep pat = filter (pat =~)

ismatch ::  String -> String -> Bool
ismatch pat inp = isJust $ matchRegex r inp 
    where r = mkRegex pat

(=~) ::  String -> String -> Bool
pat =~ inp = isJust $ matchRegex r inp 
    where r = mkRegex pat

test = subRegex (mkRegex "^F(.*)") "FooBar" "ttt"

test1 = not ((flip(=~)) "Foo" ".*o$")
test2 = not (((flip(=~)) "Foo")".*o$")
test3 = (not . ((flip(=~)) "Foo")) ".*o$"
test4 = grep ".*[ou]{2,2}\\.txt$" ["foo.txt","txt","notxt","two.txtt","oo.txt","uer.txt"]
test5 = grep "^#include[ ]{1,}" ["#include    \"mytest.h\""]

f x = 2*x
g x = 3*x
s x = f (g x) == (f . g) x


