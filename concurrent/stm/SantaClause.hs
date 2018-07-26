module Main where

import Control.Concurrent
import Control.Concurrent.STM
import System.Random

-- Gates

data Gate = MkGate Int (TVar Int)

newGate :: Int -> STM Gate
newGate n = do { tv <- newTVar 0; return (MkGate n tv) }

useGate :: Gate -> IO ()
useGate (MkGate n tv) = atomically (do { n_left <- readTVar tv; check (n_left > 0); writeTVar tv (n_left - 1) })

operateGate :: Gate -> IO ()
operateGate (MkGate n tv) = do atomically (writeTVar tv n)
                               atomically (do { n_left <- readTVar tv; check (n_left == 0) })

-- Groups

data Group = MkGroup Int (TVar (Int, Gate, Gate))

newGroup :: Int -> IO Group
newGroup n = atomically (do { g1 <- newGate n;
                              g2 <- newGate n;
                              tv <- newTVar (n, g1, g2);
                              return (MkGroup n tv) })

joinGroup :: Group -> IO (Gate, Gate)
joinGroup (MkGroup n tv) = atomically (do { (n_left, g1, g2) <- readTVar tv;
                                            check (n_left > 0);
                                            writeTVar tv (n_left - 1, g1, g2);
                                            return (g1, g2) })

awaitGroup :: Group -> STM (Gate,Gate)
awaitGroup (MkGroup n tv) = do (n_left, g1, g2) <- readTVar tv
                               check (n_left == 0)
                               new_g1 <- newGate n
                               new_g2 <- newGate n
                               writeTVar tv (n,new_g1,new_g2)
                               return (g1,g2)

-- Elves & reindeer

meetInStudy :: String -> IO ()
meetInStudy s = putStr (s ++ " meeting in the study\n")

deliverToys :: String -> IO ()
deliverToys s = putStr (s ++ " delivering toys\n")

helper1 :: Group -> IO () -> IO()
helper1 group task = do (in_gate, out_gate) <- joinGroup group
                        useGate in_gate
                        task
                        useGate out_gate

elf1      gp id = helper1 gp (meetInStudy ("Elf " ++ show id))
reindeer1 gp id = helper1 gp (deliverToys ("Reindeer " ++ show id))

forever :: IO () -> IO ()
forever act = do act
                 waitTime <- getStdRandom (randomR (1,1000000))
                 threadDelay waitTime
                 forever act

elf :: Group -> Int -> IO ThreadId
elf gp id = forkIO (forever (elf1 gp id))


reindeer :: Group -> Int -> IO ThreadId
reindeer gp id = forkIO (forever (reindeer1 gp id))

-- Santa

choose :: [(STM a, a -> IO ())] -> IO ()
choose choices = do act <- atomically (foldr1 orElse actions)
                    act
    where
      actions :: [STM (IO ())]
      actions = [ do { val <- guard; return (rhs val) } | (guard, rhs) <- choices ]

santa :: Group -> Group -> IO ()
santa elf_gp rein_gp = do putStr "----------\n"
                          choose [(awaitGroup rein_gp, run "deliver toys"),
                                  (awaitGroup elf_gp, run "meet in my study")]
    where
      run :: String -> (Gate,Gate) -> IO ()
      run what (in_gate, out_gate) = do putStr ("Ho! Ho! Ho! Let's " ++ what ++ "\n")
                                        operateGate in_gate; operateGate out_gate


-- Main

main = do elf_gp <- newGroup 3
          sequence [ elf elf_gp n | n <- [1..10]]
          rein_gp <- newGroup 9
          sequence [ reindeer rein_gp n | n <- [1..9]]
          forever (santa elf_gp rein_gp)

