{-# LANGUAGE TemplateHaskell #-}

-- | Spawning remote processes

module Main where

import Control.Concurrent(threadDelay)
import Control.Monad(forever)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Closure
import Network.Transport.TCP (createTransport, defaultTCPAddr, defaultTCPParameters)

sampleTask :: (Int, String) -> Process ()
sampleTask (t, s) = liftIO (threadDelay (t * 1000000)) >> say s

remotable ['sampleTask]

myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable

main :: IO ()
main =
  do res <- createTransport (defaultTCPAddr "127.0.0.1" "10501") defaultTCPParameters
     case res of
       Left left -> putStrLn (show left)
       Right t -> do node <- newLocalNode t myRemoteTable
                     runProcess node $
                       (do us <- getSelfNode
                           _ <- spawnLocal $ sampleTask (1 :: Int, "using spawnLocal")
                           pid <- spawn us $ $(mkClosure 'sampleTask) (1 :: Int, "using spawn")
                           liftIO $ threadDelay 2000000)
                       
