-- Implementation of a simple Token Bucket rate-limiter.
-- Mohit Cheppudira <mohit@muthanna.com>

module TokenBucket
  ( Handle
  , createHandle
  , start
  , stop
  , getToken
  , test ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.Maybe

data Bucket = Bucket {
        tokenCount :: Int,
        refillCount :: Int,
        threadID :: Maybe ThreadId
      }

newtype Handle = Handle (MVar Bucket)

createHandle :: Int -> IO Handle
createHandle rate = do
  handleMVar <- newEmptyMVar
  putMVar handleMVar Bucket { tokenCount = rate
                            , refillCount = rate
                            , threadID = Nothing }
  return $ Handle handleMVar

start :: Handle -> IO ()
start (Handle hMVar) = modifyMVar_ hMVar startRefiller
  where
    startRefiller bucket = do
      thread_id <- forkIO refillBucket
      return bucket { threadID = Just thread_id }
    refillBucket = do
      threadDelay 1000000
      thread_id <- modifyMVar hMVar (\bucket ->
        return (bucket { tokenCount = refillCount bucket },
                threadID bucket))
      case thread_id of
        Nothing -> return ()
        Just _ -> refillBucket

stop :: Handle -> IO Bool
stop (Handle hMVar) = do
  bucket <- takeMVar hMVar
  case (threadID bucket) of
    Nothing -> return False
    Just thread_id -> do
      putMVar hMVar bucket { threadID = Nothing }
      return True

getToken :: Handle -> IO (Maybe Bool)
getToken (Handle hMVar) = modifyMVar hMVar returnToken
  where returnToken bucket =
          case (threadID bucket) of
            Nothing -> return (bucket, Nothing)
            Just thread_id -> do
              let count = tokenCount bucket
              if count > 0
                then return (bucket { tokenCount = count - 1 }, Just True)
                else return (bucket, Just False)

forever :: IO a -> IO ()
forever a = do a; forever a

test :: IO ()
test = do
  -- Rate-limit at 5 operations/sec.
  handle <- createHandle 5

  -- Start token refiller
  start $ handle

  -- Stop refiller after 3 seconds
  forkIO $ do threadDelay 3000000 >> stop handle >> return ()

  -- Request a token every 100ms
  forkIO $ forever $ do
    success <- getToken handle
    print success
    threadDelay 100000

  -- Run test for 5 seconds
  threadDelay 5000000
