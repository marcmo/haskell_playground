{-# LANGUAGE OverloadedStrings #-}

import Test.Chell
import Test.Chell.QuickCheck

tests :: Suite
tests = suite "tests"
	[ suite "eq" [test_Equality]
	-- , test test_Increment
	]

-- test_Equality :: Test
test_Equality = property "equality" (\x -> x == x)

-- test_Increment :: Test
-- test_Increment = property "equality" (\x -> x + 1 > x)

main :: IO ()
main = defaultMain [tests]

