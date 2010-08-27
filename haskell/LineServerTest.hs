import Control.Concurrent.MVar
import System.IO

import LineServer

data State = State
  { payload :: String
  , counter :: MVar Int }

myConnectHandler h = do
  putStrLn "New connection"
  hPutStrLn h "HELLO"

myLineHandler state h line = do
  let ctrMVar = counter state
  ctr <- modifyMVar ctrMVar (\c -> return (c + 1, c))
  hPutStrLn h $ (payload state) ++ " [" ++ (show ctr) ++ "] : " ++ line

myErrHandler errMsg = putStrLn $ "Connection closed: " ++ errMsg

main = do
  ctr <- newMVar 0

  h <- LineServer.init $ Options
    { port = "10000"
    , connectHandler = myConnectHandler
    , lineHandler = (myLineHandler (State "hsterm" ctr))
    , errHandler = myErrHandler }

  start h
