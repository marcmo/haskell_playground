import Test.QuickCheck

-- prop_RevUnit ::  Eq a => a -> Bool
prop_RevUnit x =
    reverse x == x

-- testIt :: Testable a => a -> IO ()
testIt = quickCheck prop_RevUnit
-- usage: testIt prop_RevUnit
