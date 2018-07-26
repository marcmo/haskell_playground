{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Shell.hs
-- Copyright   :  (c) Don Stewart 2007
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  dons@cse.unsw.edu.au
-- Stability   :  stable
-- Portability :  non-portable: requires mtl, newtype deriving, pattern guards
--
-----------------------------------------------------------------------------
--
-- Provides a Shell and Priv monad, for encapulating errors in
-- shell programs nicely, and for static separation of code requiring root
-- privledges from other code.

module Shell where

import qualified Process

import System.IO
import System.Exit
import Text.Printf

import Control.Monad.Error
import Control.Exception

------------------------------------------------------------------------
--
-- The 'Shell' monad, a wrapper over IO that captures failure in an
-- error transformer.
--

newtype Shell a = Shell { runShell :: ErrorT String IO a }
    deriving (Functor, Monad, MonadIO)

--
-- The 'Priv' monad, a shell monad for commands requiring root
-- privledges. Let's us distinguish such command statically, on the type
-- level.
--
-- To run something in the Priv monad, use 'priv'.
--
newtype Priv a = Priv { priv :: Shell a }
    deriving (Functor, Monad, MonadIO)

--
-- Rather than just derive error handling, we'll roll our own that
-- propagates shell failures into errors.
--
instance MonadError String Shell where
    throwError = error . ("Shell failed: "++)
instance MonadError String Priv  where
    throwError = error . ("Priv failed: "++)

-- Run a normal shell command as the user. Return either a result or an error value
shell :: Shell a -> IO (Either String a)
shell = runErrorT . runShell

-- Run a privileged command, requiring sudo access. Return any output
runPriv :: String -> Priv String
runPriv = Priv . run . ("/usr/bin/sudo " ++)

-- ---------------------------------------------------------------------
-- Utilities

--
-- Convenient wrapper
--
io :: IO a -> Shell a
io = liftIO

--
-- Run a shell command, wrapping any errors in ErrorT
--
run :: String -> Shell String
run = io . Process.run

--
-- A 'read' returning failure in an error monad
--
readM :: (MonadError String m, Read a) => String -> m a
readM s | [x] <- parse = return x
        | otherwise    = throwError $ "Failed parse: " ++ show s
    where
        parse = [x | (x,t) <- reads s]
