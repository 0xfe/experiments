module TCPServer
  ( Options(..)
  , ServerHandle
  , TCPServer.init
  , start) where

import Network.Socket
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import System.IO
import System.IO.Error

data Options = Options
  { port :: String
  , connectHandler :: Handle -> IO () }

data Commands = Stop | Restart

data State = State
  { options :: Options
  , chan :: Chan Commands }

newtype ServerHandle = ServerHandle (MVar State)

init :: Options -> IO ServerHandle
init options = do
  chan <- newChan
  state <- newMVar $ State options chan
  return $ ServerHandle state

start :: ServerHandle -> IO ()
start (ServerHandle mvar) = withSocketsDo $
  do
    opts <- options <$> readMVar mvar
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                              Nothing (Just $ port opts)
    let serveraddr = head addrinfos
    putStrLn $ "Serving: " ++ (show serveraddr)
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bindSocket sock (addrAddress serveraddr)
    listen sock 5
    lock <- newMVar ()
    procRequests lock sock opts
  where
    procRequests lock mastersock opts =
      do (connsock, clientaddr) <- accept mastersock
         forkIO $ procLines lock connsock clientaddr opts
         procRequests lock mastersock opts
    procLines lock connsock clientaddr opts =
      do h <- socketToHandle connsock ReadWriteMode
         forkIO $ autoShutdown h
         hSetBuffering h LineBuffering
         connectHandler opts $ h
         handler h opts
         hClose h
    handler h opts =
      do messages <- try $ hGetLine h
         case messages of
           Right line -> ((lineHandler opts) h line) >> handler h opts
           Left err -> (errHandler opts) $ show err
    autoShutdown h = do
      threadDelay 15000000
      hPutStrLn h "TIMEOUT: Goodbye."
      hClose h
