
-- | Creating a node

module Main where

import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Node(initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad (forever, forM_)

main :: IO ()
main =
  do args <- getArgs

     case args of
       ["master", host, port] -> do
         backend <- initializeBackend host port initRemoteTable
         startMaster backend (master backend)

       ["slave", host, port] -> do
         backend <- initializeBackend host port initRemoteTable
         startSlave backend
         
master :: Backend -> [NodeId] -> Process ()
master backend slaves =
  do liftIO . putStrLn $ "Slaves: " ++ show slaves
     terminateAllSlaves backend

     


