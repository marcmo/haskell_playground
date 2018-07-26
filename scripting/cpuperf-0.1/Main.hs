-----------------------------------------------------------------------------
-- |
-- Module      :  cpuperf.hs
-- Copyright   :  (c) Don Stewart 2007
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  dons@cse.unsw.edu.au
-- Stability   :  stable
-- Portability :  GHC only: mtl
--
-----------------------------------------------------------------------------
--
-- Toggle the cpu speed on an openbsd machine.
-- 
-- Tries to be very robust and clean with errors, and separates
-- privledged from non-privledged shell commands on the type level
--
--  $ cpuperf
--  cpu: 100 -> 0
--
--  $ cpuperf
--  cpu: 0 -> 100
--

import Shell
import Text.Printf

main = shell $ do
    (old,new) <- modify "hw.setperf" toggle
    clock     <- get "hw.cpuspeed"
    io $ do printf "cpu: %d -> %d\n" old new
            printf "clock: %f Ghz\n" (fromIntegral clock / 1000 :: Double)

toggle v = if v == 100 then 0 else 100

-- ---------------------------------------------------------------------
--
-- A State-monad like interface to the sysctl values
--

--
-- Read a sysctl value from the shell
--
get :: String -> Shell Integer
get s = readM . parse =<< run ("sysctl " ++ s)
  where
    parse = tail . dropWhile (/= '=') . init

-- 
-- Set a sysctl value. Runs in the Priv monad, and requires root privledges.
-- Will prompt for a password.
--
set :: String -> Integer -> Priv ()
set s v = do runPriv $ printf "sysctl -w %s=%s" s (show v)
             return ()

-- 
-- Modify a particular sysctl value, using a function applied to the
-- current value, yielding a new value. Both the old and new values are
-- returned.
--
modify :: String -> (Integer -> Integer) -> Shell (Integer, Integer)
modify s f = do
    v <- get s
    let u = f v
    priv (set s u) -- root
    return (v,u)
