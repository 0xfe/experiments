-- A port scanner

module Main (main) where

import Control.Concurrent
import Control.Exception
import Data.Maybe
import Network
import Network.BSD
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [host, from, to] -> withSocketsDo $
                        scanRange host [read from .. read to]
    _                -> usage

usage = do 
  hPutStrLn stderr "Usage: scan host start_port end_port"
  exitFailure

scanRange host ports = 
    mapM (threadWithChannel . scanPort host . fromIntegral) ports >>=
    mapM_ hitCheck
  where 
    hitCheck mvar = takeMVar mvar >>= maybe (return ()) printHit
    printHit port = putStrLn =<< showService port

threadWithChannel action = do
  mvar <- newEmptyMVar
  forkIO (action >>= putMVar mvar)
  return mvar

scanPort host port =
    withDefault Nothing (tryport >> return (Just port))
  where
    tryport = connectTo host (PortNumber port) >>= hClose

showService port =
  withDefault (show port) $ do
    service <- getServiceByPort port "tcp"
    return (show port ++ " " ++ serviceName service)

withDefault defaultVal action =
  handle (const $ return defaultVal) action
