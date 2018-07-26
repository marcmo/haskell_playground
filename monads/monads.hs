
import Data.Array.IO

main = do arr <- newArray (1,10) 37 :: IO (IOArray Int Int)
          a <- readArray arr 1
          writeArray arr 1 64
          b <- readArray arr 1
          print (a, b)

main2 = do let get2chars = getChar >> getChar
           putStr "Press two keys"
           get2chars
           return ()

ioActions :: [IO ()]
ioActions = [(print "Hello!"),
             (putStr "just kidding"),
             (getChar >> return ())
            ]

sequence__ :: [IO a] -> IO ()
sequence__ [] = return ()
sequence__ (x:xs) = do x
                       sequence__ xs

main3 = do head ioActions
           ioActions !! 1
           last ioActions

main4 = sequence__ ioActions
