import Control.Monad.Reader

doThis :: ReaderT String IO ()
doThis = do
  val <- ask
  liftIO . putStrLn $ val

main = do
  runReaderT doThis "Boo"
