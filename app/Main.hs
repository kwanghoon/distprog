module Main where

import Network.Transport.TCP (createTransport, defaultTCPAddr, defaultTCPParameters)
import Control.Distributed.Process
import Control.Distributed.Process.Node

main :: IO ()
main =
  do res <- createTransport (defaultTCPAddr "127.0.0.1" "10501") defaultTCPParameters
     case res of
       Left left -> putStrLn (show left)
       Right t -> do 
                     node <- newLocalNode t initRemoteTable
                     _ <- runProcess node $
                       (do self <- getSelfPid
                           send self "hello"
                           send self "wow!!"
                           hello <- expect :: Process String
                           liftIO $ putStrLn hello
                           wow <- expect :: Process String
                           liftIO $ putStrLn wow
                       )
                     return ()


