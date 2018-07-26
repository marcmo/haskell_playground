module Main  where

import Nvram
import NvramCompatibility
import VerifyCompatiblityBlocks
import System(getArgs)
import qualified Test.Framework as TF -- (defaultMainWithOpts, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit

-- example usage: runghc Main.hs
defaultWorkspace="/home/omueller/dev/git/BDC2/"

main ::  IO ()
main = do 
  args <- getArgs
  let (workspace:_) = if (length args == 1) then args else [defaultWorkspace]
  let nvmConfig=workspace++"configuration/model/nvram/NvmConfigBDC.pgm"
  let compatibilityConfig=workspace++"configuration/model/nvram/NvmCompatibility.pgm"
  putStrLn $ "taking nvm-config: " ++ nvmConfig ++ ", compatibilityConfig: " ++ compatibilityConfig
  nvmConfig <- readFile nvmConfig
  compatConfig <- readFile compatibilityConfig
  case parseInput nvmConfig of 
    Left err -> do  putStrLn "Error parsing nvmConfig:"
                    print err
    Right nvmConf -> case parseCompatibilityConfig compatConfig of
                  Left err2 -> do  putStrLn "Error parsing compatibility config:"
                  Right nvmCompConf -> do
                    print $ compareLength nvmCompConf nvmConf
                    TF.defaultMainWithArgs (tests nvmCompConf nvmConf) []

tests :: NvramCompatibilityConfig-> NvramConfig-> [TF.Test]
tests nvmCompConf nvmConf = [
        TF.testGroup "Check Group 1" [
                testCase "checkNoSecret" (test_noSecret nvmCompConf nvmConf),
                testCase "checkCorrectOffsets" (test_CorrectOffsets nvmCompConf),
                testCase "checkCorrectLength" (test_correctLength nvmCompConf nvmConf)
            ]
    ]

test_noSecret nvmCompConf nvmConf = let unmatching = checkNoSecret nvmCompConf nvmConf in
    assertBool ("check that no secret blocks are in compatibility configuration: " ++ displayBlocks "secret block in compatibiliyt configuration " unmatching) $ null unmatching
test_CorrectOffsets nvmCompConf = let unmatching = checkCorrectOffsets nvmCompConf in
    assertBool ("check that the specified offsets are correct" ++ displayBlocks "blocks should be next to each other:" unmatching) $ null unmatching 
test_correctLength nvmCompConf nvmConf = let offending = checkCorrectLength nvmCompConf nvmConf in
    assertBool ("check that all specified length in compatibility file are correct: " ++ displayBlocks "specified length is not correct:" offending) $ null offending 

displayBlocks msg xs = if null xs then msg else msg ++ show (head xs)
