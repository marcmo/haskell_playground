module Main (main) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import System.Random

data Species a = Reindeer [a] | Elves [a]

main = do
  (toRobin,fromRobin) <- secretary 9 Reindeer
  (toEdna,fromEdna) <- secretary 3 Elves
  sequence [ spawnWorker (atomically . toRobin) "Reindeer " i " delivering toys."
           | i <- [1..9] ]
  sequence [ spawnWorker (atomically . toEdna) "Elf " i " meeting in the study."
           | i <- [1..10] ]
  newChan >>= santa (atomically (fromRobin `orElse` fromEdna)) -- main thread is santa's

secretary count species = do
  chan <- newTChanIO
  return (writeTChan chan,fmap species (replicateM count (readTChan chan)))

santa getNext self = forever (getNext >>= handle) where
  handle (Reindeer group) = do putStrLn "Ho, ho, ho!  Let's deliver toys!"
                               act group
  handle (Elves group)    = do putStrLn "Ho, ho, ho!  Let's meet in the study!"
                               act group
  act group = do sequence_ [tellMember (writeChan self ()) | tellMember <- group]
                 replicateM_ (length group) (readChan self)

spawnWorker tellSecretary before i after =
  forkIO (newChan >>= worker tellSecretary (before ++ show i ++ after))

worker tellSecretary msg self = forever $ do
  threadDelay =<< randomRIO (0,1000*1000) -- 0 to 1 second
  tellSecretary (writeChan self)
  tellGateKeeperIamDone <- readChan self
  putStrLn msg
  tellGateKeeperIamDone
