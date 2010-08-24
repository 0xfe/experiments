-- Implementation of a simple Token Bucket rate-limiter.
-- Mohit Cheppudira <mohit@muthanna.com>

module TokenBucket where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.Maybe

data Bucket = Bucket {
        tbTokenCount :: Int,
        tbRefillCount :: Int,
        tbThreadID :: Maybe ThreadId
      }

data Handle = Handle (MVar Bucket)

forever a = do a; forever a

init rate = do
  handleMVar <- newEmptyMVar
  putMVar handleMVar Bucket { tbTokenCount = rate
                            , tbRefillCount = rate
                            , tbThreadID = Nothing }
  return $ Handle handleMVar

start (Handle hMVar) = do
    bucket <- takeMVar hMVar
    thread_id <- forkIO $ do refillBucket
    putMVar hMVar (bucket { tbThreadID = Just thread_id })
  where refillBucket = do
          threadDelay 1000000
          bucket <- takeMVar hMVar
          putMVar hMVar (bucket { tbTokenCount = tbRefillCount bucket })
          case (tbThreadID bucket) of
            Nothing -> return ()
            Just _ -> refillBucket

stop (Handle hMVar) = do
  bucket <- takeMVar hMVar
  case (tbThreadID bucket) of
    Nothing -> return False
    Just thread_id -> do
      putMVar hMVar bucket { tbThreadID = Nothing }
      return True

getToken (Handle hMVar) = modifyMVar hMVar returnToken
  where returnToken bucket =
          case (tbThreadID bucket) of
            Nothing -> return (bucket, Nothing)
            Just thread_id -> do
              let count = tbTokenCount bucket
              if count > 0
                then return (bucket { tbTokenCount = count - 1 }, Just True)
                else return (bucket, Just False)

test = do
  -- Rate-limit at 5 operations/sec.
  handle <- TokenBucket.init 5
  start $ handle

  forkIO $ do threadDelay 3000000 >> stop handle >> return ()

  -- Request a token every 100ms. Only 5 should succeed a second.
  forever $ do
    success <- getToken handle
    print success
    threadDelay 100000
