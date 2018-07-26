import System.IO
import Data.Char
import Control.Applicative

main = server stdin wordReverser
-- newtype Application = App { runApp :: Char -> IO Application }
-- 
-- 
-- server :: Handle -> Application -> IO ()
-- server input app = do
--     c    <- hGetChar input
--     app' <- runApp app c
--     server input app'
-- 
-- wordReverser :: Application
-- wordReverser = App (go [])
--     where go xs c | not (isSpace c) = return (App (go (c:xs)))
--                   | null xs         = return wordReverser
--                   | otherwise       = putStrLn xs >> return wordReverser

-- data Application = Enough  (IO ())
--                  | Partial (Char -> Application)
-- 
-- server :: Handle -> Application -> IO ()
-- server input (Enough a)  = a
-- server input (Partial f) = do
--     c <- hGetChar input
--     server input (f c)
-- 
-- wordReverser :: Application
-- wordReverser = Partial (go [])
--     where go xs c | not (isSpace c) = Partial (go (c:xs))
--                   | null xs         = Enough (return ())
--                   | otherwise       = Enough (putStrLn xs)
data Input = EOF | More Char

data Application = Enough  (IO ())
                 | Partial (Input -> Application)

server :: Handle -> Application -> IO ()
server input (Enough a)  = a
server input (Partial f) = do
    eof <- hIsEOF input
    c   <- if eof then return EOF else More <$> (hGetChar input)
    server input (f c)

wordReverser :: Application
wordReverser = Partial (go [])
    where go xs (More c) | not (isSpace c) = Partial (go (c:xs))
                         | otherwise       = finish xs
          go xs EOF                        = finish xs

          finish []                        = Enough (return ())
          finish xs                        = Enough (putStrLn xs)

