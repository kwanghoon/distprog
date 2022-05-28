
-- | Sending messages

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.TCP (createTransport, defaultTCPAddr, defaultTCPParameters)


replyBack :: (ProcessId, String) -> Process ()
replyBack (sender, msg) = send sender msg

logMessage :: String -> Process ()
logMessage msg = say $ "handling " ++ msg


main :: IO ()
main =
  do res <- createTransport (defaultTCPAddr "127.0.0.1" "10501") defaultTCPParameters
     case res of
       Left left -> putStrLn (show left)
       Right t -> do 
                     node <- newLocalNode t initRemoteTable
                     _ <- runProcess node $
                       (do echoPid <- spawnLocal $ forever $
                             (do receiveWait [match logMessage, match replyBack])

                           say "send some message!"
                           send echoPid "hello"

                           self <- getSelfPid
                           send echoPid (self, "hello")

                           m <- expectTimeout 1000000
                           case m of
                             Nothing -> die "nothing came back!"
                             Just s -> say $ "got " ++ s ++ " back!"
                             
                           liftIO $ threadDelay 2000000
                       )
                     return ()


