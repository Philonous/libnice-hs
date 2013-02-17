{-# Language ForeignFunctionInterface #-}

module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Network.Socket

import System.Glib.Attributes
import System.Glib.GType
import System.Glib.MainLoop
import System.Glib.Signals

import Network.Ice.NiceAgent
import Network.Ice.NiceCandidate
import Network.Ice.Utils

main = do
    putStrLn "test running"
    glibTypeInit
--    niceDebugEnable True
    ml <- mainLoopNew Nothing True
    putStrLn "loop is running"
    ag <- niceAgentNew CompatibilityRfc5245 (mainLoopGetContext ml)
    set ag [ stunServer := "132.177.123.6"
           , stunServerPort := 3478
           ]
    -- setStun ag
    putStrLn "set done"
    gatherWait <- newEmptyMVar
    on ag candidateGatheringDone (\i -> do
                                       putStrLn "cg done"
                                       print i
                                       putStrLn "all candidats printed"
                                       putMVar gatherWait ()
                                 )
    niceAgentAddStream ag 1
    attachReceive ag 1 1 (mainLoopGetContext ml) (\_ -> return ())
    niceAgentGatherCandidates ag 1
    forkIO $ mainLoopRun ml
    putStrLn "main loop running"
    takeMVar gatherWait
    cands <- niceAgentGetLocalCandidates ag 1 1
    mapM print cands
    -- cbCandidateGatheringDone ag 1


testCandidate = NiceCandidate { candidateType = CandidateTypeHost
                              , candidateTransport = CandidateTransportUdp
                              , address = SockAddrInet 123 1
                              , baseAddress = SockAddrInet 321 145
                              , priority = 99
                              , streamId = 1
                              , componentId = 1
                              , foundation = "bla"
                              , username = Just "un"
                              , password = Just "pwd"
                              , turn = nullPtr
                              , sockPtr = nullPtr
                              }

main' = do
    with testCandidate $ \p -> print =<< peek p

--    mainLoopRun ml
