-- EvilBot: Foul mouthed evil IRC bot.
-- Author: Mohit Cheppudira (mmuthanna@google.com)

import Network
import Maybe
import System.IO
import Text.Printf
import Text.Regex.Posix

-- This is an IRC module and a tiny examply app, rolled into one. It will
-- eventually be separated.

-- Public types exported by module.

type IRCServer = String
type IRCPort = Integer

data IRCChannelHandle = IRCChannelHandle {
        ircHandle :: Maybe Handle,
        ircServer :: IRCServer,
        ircPort :: IRCPort,
        ircChannel :: Maybe String,
        ircNick :: String,
        ircUser :: String,
        ircFullName :: String
     } deriving (Show)

data IRCMessage = IRCMessage {
        ircMessageNick :: String,
        ircMessageChannel :: String,
        ircMessageText :: String
     } deriving (Show)

type IRCMessageHandler = IRCChannelHandle -> IRCMessage -> Maybe String

-- Public functions exposed by this module

ircOpenChannel :: IRCServer -> IRCPort -> String -> String -> String ->
                  String -> IO IRCChannelHandle
ircOpenChannel server port chan nick user fullname = do
  handle <- openConnection server port
  h <- ircSetNick handle nick
  h' <- ircSetUser h user fullname
  h'' <- ircJoinChannel h' chan
  return h''

ircRead :: IRCChannelHandle -> IO String
ircRead h = hGetLine $ fromJust $ ircHandle h

ircWrite :: IRCChannelHandle -> String -> String -> IO ()
ircWrite h s t = do
  hPrintf (fromJust $ ircHandle h) "%s %s\r\n" s t
  printf "> %s %s\n" s t

ircPong :: IRCChannelHandle -> String -> IO ()
ircPong h s = do
  if isJust $ getPing s
    then do
      ircWrite h "PONG" $ ":" ++ (fromJust $ getPing s)
    else return ()

ircSetUser :: IRCChannelHandle -> String -> String -> IO IRCChannelHandle
ircSetUser h user fullname = do
  ircWrite h "USER" (user ++ " " ++ user ++ " irc :" ++ fullname)
  return h { ircUser = user, ircFullName = fullname }

ircSetNick :: IRCChannelHandle -> String -> IO IRCChannelHandle
ircSetNick h nick = do
  ircWrite h "NICK" nick
  s <- ircRead h
  ircPong h s
  return h { ircNick = nick }

ircJoinChannel :: IRCChannelHandle -> String -> IO IRCChannelHandle
ircJoinChannel h channel = do
  ircWrite h "JOIN" channel
  return h { ircChannel = Just channel }

ircStart :: IRCChannelHandle -> IRCMessageHandler -> IO ()
ircStart h mh = forever $ do
    s <- ircRead h
    ircPong h s
    if isJust $ getPrivMsg s
      then reply (mh h (fromJust $ getPrivMsg s))
      else return ()
  where
    forever a = do a; forever a
    reply msg = do
      if isJust msg
        then ircWrite h "PRIVMSG" ((fromJust $ ircChannel h) ++ " :" ++
                                  (fromJust msg))
        else return ()

-- Internal Parsers used by this module
openConnection :: String -> Integer -> IO IRCChannelHandle
openConnection server port = do
  h <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering
  return IRCChannelHandle { ircHandle = Just h,
                            ircServer = server,
                            ircPort = port,
                            ircChannel = Nothing,
                            ircNick = "nobody",
                            ircUser = "nobody",
                            ircFullName = "No One" }

getPing :: String -> Maybe String
getPing s = value $ ((s =~ "^PING :(.+)") :: (String, String, String, [String]))
  where value (_, _, _, []) = Nothing
        value (_, _, _, matches) = Just (matches !! 0)

getPrivMsg :: String -> Maybe IRCMessage
getPrivMsg s = values ((s =~ "^:(.+)!.+ PRIVMSG (#.+) :(.+)$") ::
                (String, String, String, [String]))
  where values (_, _, _, []) = Nothing
        values (_, _, _, matches) =
          Just (IRCMessage (matches!!0) (matches!!1) (matches!!2))

-- This application... which will be eventially separated out of the
-- module.

snarkyBastard :: IRCMessageHandler
snarkyBastard h m
  | msg =~ ("^" ++ mynick ++ ":") =
        Just (fromnick ++ ": don't talk to me, minion")
  | msg == "wtf?\r" = Just "your mom"
  | msg == "yes\r" = Just "no"
  | msg == "no\r" = Just "yes"
  | msg == "wtf?\r" = Just "your mom"
  | msg =~ mynick =
        Just (fromnick ++ ": don't talk about your master that way, scum")
  | msg =~ "philhutton" = Just "that philhutton is a real cheeky bastard"
  | msg =~ "dorland" = Just "are you talking to a croissant again?"
  | msg =~ "haskell" = Just "haskell made me what i am"
  | msg =~ "food" = Just "mmm... chips..."
  | msg =~ " +mom *" = Just "it's not nice to make fun of people's moms"
  | otherwise = Nothing
  where
    msg = ircMessageText m
    fromnick = ircMessageNick m
    mynick = ircNick h

main :: IO ()
main = do
  let server = "irc"
  let port = 6667
  let channel = "#mmuthanna"
  let nick = "haskellbot"
  let username = "mmuthanna"
  let fullname = "Mohit Cheppudira"

  -- Start being a bastard
  h <- ircOpenChannel server port channel nick username fullname
  ircStart h snarkyBastard
