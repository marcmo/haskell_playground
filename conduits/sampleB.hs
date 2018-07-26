import Data.Conduit
import Control.Monad.IO.Class
import qualified Data.Conduit.List as CL

source :: Source IO Int
source = do
    yield 1
    yield 2
    yield 3
    yield 4

sourceList :: Monad m => [a] -> Source m a
sourceList = mapM_ yield

-- multiplyConduit :: Conduit [Int] IO [Int]
-- multiplyConduit = do
--     x <- await
--     case x of
--       Nothing -> return ()
--       Just r -> CL.map (*r)
conduit :: Conduit Int IO String
conduit = do
    -- Get all of the adjacent pairs from the stream
    mi1 <- await
    mi2 <- await
    case (mi1, mi2) of
        (Just i1, Just i2) -> do
            yield $ show (i1, i2)
            leftover i2
            conduit
        _ -> return ()

sink :: Sink String IO ()
sink = do
    mstr <- await
    case mstr of
        Nothing -> return ()
        Just str -> do
            liftIO $ putStrLn str
            sink

sink2 :: Sink String IO ()
sink2 = awaitForever $ liftIO . putStrLn

myAwaitForever :: Monad m => (a -> Conduit a m b) -> Conduit a m b
myAwaitForever f = await >>= maybe (return ()) (\r -> f r >> myAwaitForever f)

main = do
  source $$ conduit =$ sink
  sourceList [20..24] $$ conduit =$ sink
