
myPutString::String -> IO()
myPutString[] = return()
myPutString(x:xs) = putChar x >> myPutString xs

echo::IO()
echo = getChar >>= putChar

myGets::Int -> IO String
myGets 0 = return []
myGets(n+1) = getChar >>= \x -> myGets n >>= \xs -> return (x:xs)

myGets2::Int -> IO String
myGets2 0 = return []
myGets2 (n+1) = do x <- getChar
                   xs <- myGets2 n
                   return(x:xs)
