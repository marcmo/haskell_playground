import Data.List

type Callback = IO()
data VoltageListener = VL Int Callback
instance Show VoltageListener 
  where show (VL n _) = show n
instance Eq VoltageListener 
  where (VL n _) == (VL n2 _) = n == n2
u1=VL 2 (print "called u2")
u2=VL 6 (print "called u6")
o1=VL 14 (print "called o14")
o2=VL 16 (print "called o16")
underVoltageListeners=[u1,u2]
overVoltageListeners=[o1,o2]

voltageChanged ::  Int -> Int -> IO ()
voltageChanged new old 
  | new < old = callUnderVoltageListeners new old
  | otherwise = callOverVoltageListeners new old

callUnderVoltageListeners new old = do
  let updates = [x | x@(VL n _) <- underVoltageListeners, new < n, new < old, old > n]
  mapM_ (\(VL _ f)->f) updates
  
callOverVoltageListeners new old = do
  let updates = [x | x@(VL n _) <- overVoltageListeners, new > n, new > old, old < n]
  mapM_ (\(VL _ f)->f) updates
