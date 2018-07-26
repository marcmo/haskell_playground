import qualified Text.Regex.Posix as P

-- test = "foo" =~ "o" :: [[String]]
-- 
-- test2 = matchAll s "Foo"
--   where s = makeRegex "o" :: Regex
-- 
-- test3 = "my left foot" =~ "foo" :: Bool

-- (=~) :: String -> String -> Bool
(=~) a b = a P.=~ b :: Bool

