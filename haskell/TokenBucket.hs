-- Implementation of a simple Token Bucket rate-limiter.
-- Mohit Cheppudira <mohit@muthanna.com>

module TokenBucket where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.Maybe

data TokenBucket = TokenBucket {
        tbTokenCount :: MVar Int,
        tbRefillCount :: Int,
        tbThreadID :: MVar ThreadId
      }

forever a = do a; forever a

init rate = do
  countMVar <- newMVar rate
  threadIDMVar <- newEmptyMVar
  return TokenBucket { tbTokenCount = countMVar
                     , tbRefillCount = rate
                     , tbThreadID = threadIDMVar }

start tb = do
    thread_id <- forkIO $ forever $ do
      threadDelay 1000000
      swapMVar (tbTokenCount tb) (tbRefillCount tb)
    putMVar (tbThreadID tb) thread_id

stop tb = do
  thread_id <- takeMVar (tbThreadID tb)
  killThread thread_id

getToken tb = do
  thread_id <- readMVar (tbThreadID tb)
  count <- takeMVar countMVar
  if count > 0
    then do putMVar countMVar (count - 1); return True
    else do putMVar countMVar 0; return False
  where countMVar = tbTokenCount tb

test = do
  -- Rate-limit at 5 operations/sec.
  tb <- TokenBucket.init 5
  start $ tb

  -- Request a token every 100ms. Only 5 should succeed a second.
  forever $ do
    success <- getToken tb
    print success
    threadDelay 100000

  stop $ tb
