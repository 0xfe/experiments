module LineServer
  ( Options(..)
  , LSHandle
  , LineServer.init
  , start) where

import Network.Socket
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad.Reader
import System.IO
import System.IO.Error

type LineHandler = Handle -> String -> IO ()
type ErrorHandler = String -> IO ()

data Options = Options
  { port :: String
  , connectHandler :: Handle -> IO ()
  , lineHandler :: LineHandler
  , errHandler :: ErrorHandler }

data Commands = Stop | Restart

data State = State
  { options :: Options
  , chan :: Chan Commands }

newtype LSHandle = LSHandle (MVar State)

init :: Options -> IO LSHandle
init options = do
  chan <- newChan
  state <- newMVar $ State options chan
  return $ LSHandle state

start :: LSHandle -> IO ()
start (LSHandle mvar) = withSocketsDo $
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
    runReaderT (procRequests lock sock) opts
  where
    procRequests :: MVar () -> Socket -> ReaderT Options IO ()
    procRequests lock mastersock =
      do opts <- ask
         (connsock, clientaddr) <- liftIO $ accept mastersock
         liftIO . forkIO $ runReaderT (procLines lock connsock clientaddr) opts
         procRequests lock mastersock
    procLines :: MVar () -> Socket -> a -> ReaderT Options IO ()
    procLines lock connsock clientaddr =
      do opts <- ask
         liftIO $ do
           h <- socketToHandle connsock ReadWriteMode
           forkIO $ autoShutdown h
           hSetBuffering h LineBuffering
           (connectHandler opts) h
           handler (lineHandler opts) (errHandler opts) h
           hClose h
    handler :: LineHandler -> ErrorHandler -> Handle -> IO ()
    handler line_handler error_handler h =
      let retry = handler line_handler error_handler h in
      do message <- try $ hGetLine h
         case message of
           Right line -> (line_handler h line) >> retry
           Left err -> error_handler (show err)
    autoShutdown :: Handle -> IO ()
    autoShutdown h = do
      threadDelay 15000000
      hPutStrLn h "TIMEOUT: Goodbye."
      hClose h
