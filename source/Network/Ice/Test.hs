module Main where

import Network.Ice.NiceAgent
import System.Glib.Signals
import System.Glib.GType
import System.Glib.MainLoop

main = do
    putStrLn "test running"
    glibTypeInit
--    ml <- mainLoopNew Nothing False
    putStrLn "main loop running"
    ag <- newNiceAgent CompatibilityRfc5245
    on ag candidateGatheringDone (\i -> putStrLn "cg done" >> print i)
    niceAgentAddStream ag 1
    niceAgentGatherCandidates ag 1
--    mainLoopRun ml
