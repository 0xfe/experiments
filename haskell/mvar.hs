import Control.Concurrent
import Control.Monad
import Ix (range)

communicate :: Int -> IO ()
communicate n = do
  m <- newEmptyMVar

  thread_list <- forM (range (1, n)) $ \i -> forkIO $ putMVar m i

  putStrLn $ "Started threads: " ++ show thread_list
  
  replicateM_ n $ do
    v <- takeMVar m
    putStrLn $ "Got: " ++ show v

main :: IO ()
main = communicate 10
