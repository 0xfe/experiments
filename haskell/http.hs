import Control.Monad
import Control.Applicative
import Data.Maybe
import Network.HTTP
import Network.URI
import System (getArgs)

downloadURL :: String -> IO (Either String String)
downloadURL url =
  do resp <- simpleHTTP request
     case resp of
      Left x -> return $ Left $ "Error: " ++ show x
      Right r ->
        case rspCode r of
          (2, _, _) -> return $ Right (rspBody r)
          _ -> return $ Left $ "Error: " ++ show r
  where request = Request { rqURI = uri,
                            rqMethod = GET,
                            rqHeaders = [],
                            rqBody = "" }
        uri = fromJust $ parseURI url

wget :: String -> IO String
wget url = do
  body <- downloadURL url
  case body of
    Left x -> return x
    Right r -> return r

main :: IO ()
main = do
  args <- getArgs
  if length args == 0
    then putStrLn "Usage: http.hs url"
    else putStrLn =<< (wget $ args !! 0)
        
