import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Ix

chanExample = do
  -- Create channel
  chan <- newChan

  -- Spawn 10 threads and send messages to the channel.
  forM_ (range (1,10)) (\i -> forkIO $ writeChan chan (show i))

  -- Read messages from the channel.
  replicateM_ 10 (readChan chan >>= print)

main = chanExample
