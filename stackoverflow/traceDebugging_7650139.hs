import System.IO.Unsafe

foo :: Int -> Bool
foo x 
  | x > 0 = debug "ok" True
  | otherwise = debug "ohhh...no" False

-- debugIO :: String -> Bool -> IO Bool
debugIO s r = putStrLn s >> return r

debug :: String -> Bool -> Bool
debug s r = unsafePerformIO $ debugIO s r

