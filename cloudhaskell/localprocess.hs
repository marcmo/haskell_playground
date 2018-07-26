{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Local where

import Control.Monad
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Data.Binary
import Data.Typeable

newtype Ping = Ping ProcessId deriving (Typeable)

instance Binary Ping where
    put (Ping pid) = put pid
    get = do pid <- get
             return $ Ping pid

ping :: Process ()
ping = do
    self <- getSelfPid
    Ping partner <- expect
    send partner (Ping self)
    say "ping!"
    ping

remotable ['ping] -- Template Haskell magic

-- pingClosure :: Closure (Process ())
-- pingClosure = $(mkClosure 'ping)

initialProcess ::  Process ()
initialProcess = do
    nid <- getSelfNode
    pid <- getSelfPid
    (sendP,receiveP) <- newChan :: Process (SendPort Ping, ReceivePort Ping)
    sendChan sendP (Ping pid)
    -- ping1 <- spawn nid ping__closure
    -- ping2 <- spawn nid ping__closure
    -- send ping1 (Ping ping2)
    --   where ping__closure = ($(mkClosure 'ping) ())


-- main = remoteInit (Just "config") [__remoteCallMetaData] initialProcess



