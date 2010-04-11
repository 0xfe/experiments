import Data.Bits
import Network.Socket
import Network.BSD
import Data.List

type HandlerFunc = SockAddr -> String -> IO ()

getAddress port = do
    addressinfos <- addresses
    return $ head addressinfos
  where addresses = getAddrInfo
                 (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                 Nothing (Just port)

udpServer :: String -> HandlerFunc -> IO ()
udpServer port handlerfunc = withSocketsDo $
  do
    serveraddr <- getAddress port
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    bindSocket sock (addrAddress serveraddr)
    procMessages sock
  where procMessages sock =
          do
            (msg, _, addr) <- recvFrom sock 1024
            handlerfunc addr msg
            procMessages sock


plainHandler :: HandlerFunc
plainHandler addr msg =
  putStrLn $ "From " ++ show addr ++ ": " ++ msg

main = do
  let portNum = "8888"
  serveraddr <- getAddress portNum
  putStrLn $ "Listening on " ++ (show $ addrAddress serveraddr) ++ "/udp"
  udpServer portNum plainHandler
