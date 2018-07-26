{-# LANGUAGE DeriveDataTypeable #-}
import Control.Exception
import Data.Typeable(Typeable)
import System.Environment(getArgs)

data MyException = ThisException | ThatException
    deriving (Show, Typeable)

instance Exception MyException


foo :: Int -> IO ()
foo n
  | n > 5 = throw (userError "not possible")
  | otherwise = print "everything ok"

main = do
    n:_ <- getArgs
    catch (foo (read n))
         (\e -> do let err = show (e :: IOException)
                   print ("Warning: " ++ err)
                   return ())
    print "and we are done..."

