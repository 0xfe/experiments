-- Implementation of a simple Token Bucket rate-limiter.
-- Mohit Cheppudira <mohit@muthanna.com>

import Control.Concurrent
import Control.Concurrent.MVar
import Data.Maybe

data TokenBucket = TokenBucket {
        tokenCount :: MVar Int,
        refillCount :: Int,
        threadID :: MVar ThreadId
      }

forever a = do a; forever a

init rate = do
  countMVar <- newMVar rate
  threadIDMVar <- newEmptyMVar
  return TokenBucket { tokenCount = countMVar
                     , refillCount = rate
                     , threadID = threadIDMVar }

start tb = do
    thread_id <- forkIO $ forever $ do
      threadDelay 1000000
      swapMVar (tokenCount tb) (refillCount tb)
    putMVar (threadID tb) thread_id

stop tb = do
  thread_id <- takeMVar (threadID tb)
  killThread thread_id

getToken tb = do
  thread_id <- readMVar (threadID tb)
  count <- takeMVar countMVar
  if count > 0
    then do putMVar countMVar (count - 1); return True
    else do putMVar countMVar 0; return False
  where countMVar = tokenCount tb

main = do
  tb <- Main.init 5
  start $ tb
  forever $ do
    success <- getToken tb
    print success
    threadDelay 100000
  stop $ tb
