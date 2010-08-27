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
import Control.Monad.Error
import Data.Maybe
import System.IO.Error

data Bucket = Bucket {
        tokenCount :: Int,
        refillCount :: Int,
        refillRate :: Integer,
        threadID :: Maybe ThreadId
      }

newtype Handle = Handle (MVar Bucket)

-- Create and return a handle to a rate-limiter, which refills
-- 'count' tokens every 'rate' microseconds. For example:
--
--      handle <- createHandle 250 1000000
--
-- The above incantation returns a ratelimiter that produces tokens
-- at the rate of 250 tokens/sec.
createHandle :: Int -> Integer -> IO Handle
createHandle count rate = do
  handleMVar <- newEmptyMVar
  putMVar handleMVar Bucket { tokenCount = count
                            , refillCount = count
                            , refillRate = rate
                            , threadID = Nothing }
  return $ Handle handleMVar

-- Start the token-refill thread. This must be called before calling
-- getToken.
--
-- Fires an exception if the thread is already running.
start :: Handle -> IO ()
start (Handle hMVar) = modifyMVar_ hMVar startRefiller
  where
    startRefiller bucket = do
      case threadID bucket of
        Nothing -> do
          thread_id <- forkIO $ refillBucket (refillRate bucket)
          return bucket { threadID = Just thread_id }
        Just _ -> throwIO "Already started"
    refillBucket rate = do
      threadDelay $ fromIntegral rate
      thread_id <- modifyMVar hMVar (\bucket ->
        return (bucket { tokenCount = refillCount bucket },
                threadID bucket))
      case thread_id of
        Nothing -> return ()
        Just _ -> refillBucket rate


-- Stop the token-refill thread. Returns False if the thread was never
-- started.
stop :: Handle -> IO Bool
stop (Handle hMVar) = modifyMVar hMVar stopThread
  where stopThread bucket =
          case (threadID bucket) of
            Nothing -> return (bucket, False)
            Just thread_id -> return (bucket { threadID = Nothing }, True)

-- Request a token. Tokens are delivered at the rate requested during
-- creation.
--
-- Returns:
--
--   Nothing - The token-refill thread is not running. Call 'start'.
--   Just True  - The token was approved.
--   Just False - Out of tokens for this round. Try again soon.
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

secToUsec :: Num a => a -> a
secToUsec = (1000000 *)

test :: IO ()
test = do
  -- Rate-limit at 5 operations/sec.
  h <- createHandle 5 $ secToUsec 1

  -- Start token-refill thread
  start h
  err <- try (start h)
  case err of
    Left e -> putStrLn $ show e
    Right _ -> return ()


  -- Stop token-refill thread after 3 seconds.
  forkIO $ do threadDelay (secToUsec 3) >> stop h >> return ()

  -- Request a token every 100ms.
  forkIO $ forever $ do
    success <- getToken h
    print success
    threadDelay 100000

  -- Run test for 5 seconds.
  threadDelay $ secToUsec 5
