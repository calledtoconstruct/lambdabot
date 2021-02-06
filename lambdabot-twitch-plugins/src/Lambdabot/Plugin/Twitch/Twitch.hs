{-# OPTIONS_GHC -fno-warn-overlapping-patterns -Wmissing-signatures #-}

module Lambdabot.Plugin.Twitch.Twitch (twitchPlugin) where

import Lambdabot.Config.Twitch (reconnectDelay)
import Lambdabot.IRC (IrcMessage (..), joinChannel, pass, setNick, user, MessageDirection (Inbound, Outbound))
import Lambdabot.Logging (MonadLogging, debugM, errorM, infoM)
import Lambdabot.Monad (
  IRCRWState (ircChannels, ircPersists, ircServerMap),
  MonadLB (lb),
  received,
  registerServer,
  send,
  unregisterServer,
 )
import Lambdabot.Plugin (
  Command (help, privileged, process),
  LB,
  Module (moduleCmds, moduleDefState),
  ModuleT,
  MonadConfig (getConfig),
  Nick (..),
  command,
  getCN,
  modifyMS,
  newModule,
  readM,
  readMS,
  readNick,
  say,
  withMS,
 )
import Lambdabot.Util (io)

import Control.Concurrent.Lifted (
  MVar,
  fork,
  forkFinally,
  killThread,
  newEmptyMVar,
  putMVar,
  takeMVar,
  threadDelay,
 )
import qualified Control.Concurrent.SSem as SSem
import Control.Exception (SomeException (..))
import Control.Exception.Lifted as E (catch, throwIO)
import Control.Monad (forM_, forever, void, when)
import Control.Monad.State (gets, modify)
import Control.Monad.Trans (MonadTrans (lift))
import qualified Data.ByteString.Char8 as P
import Data.List (isPrefixOf, elemIndex)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Typeable (typeOf)
import Network (HostName, PortID (..), connectTo)
import System.IO (
  BufferMode (NoBuffering),
  Handle,
  hClose,
  hGetLine,
  hSetBuffering,
 )
import System.Timeout.Lifted (timeout)
import Data.Maybe (fromJust)

data TwitchState = TwitchState
  { password :: Maybe String
  , writable :: [String]
  }

type Twitch = ModuleT TwitchState LB

twitchPlugin :: Module TwitchState
twitchPlugin =
  newModule
    { moduleCmds =
        return
          [ (command "twitch-connect")
              { privileged = True
              , help = say "twitch-connect tag host portnum nickname userinfo.  connect to a twitch server"
              , process = \rest -> case splitOn " " rest of
                  tag : hostn : portn : nickn : uix -> do
                    pn <- (PortNumber . fromInteger) `fmap` readM portn
                    lift (online tag hostn pn nickn (unwords uix))
                  _ -> say "Not enough parameters!"
              }
          , (command "twitch-persist-connect")
              { privileged = True
              , help = say "twitch-persist-connect tag host portnum nickname userinfo.  connect to a twitch server and reconnect on network failures"
              , process = \rest -> case splitOn " " rest of
                  tag : hostn : portn : nickn : uix -> do
                    pn <- (PortNumber . fromInteger) `fmap` readM portn
                    lift (online tag hostn pn nickn (unwords uix))
                    lift $
                      lift $
                        modify $ \state' ->
                          state'
                            { ircPersists = M.insert tag True $ ircPersists state'
                            }
                  _ -> say "Not enough parameters!"
              }
          , (command "twitch-auth")
              { privileged = True
              , help = say "twitch-auth oauth.  set oauth for next twitch-connect command"
              , process = \rest -> case splitOn " " rest of
                  pwd : _ -> modifyMS (\ms -> ms{password = Just pwd})
                  _ -> say "Not enough parameters!"
              }
          , (command "twitch-interactive")
              { privileged = True
              , help = say "twitch-interactive <channel>.  allow bot to respond in specified channel"
              , process = \rest -> modifyMS (\ms -> ms{writable = rest : filter (/= rest) (writable ms)})
              }
          , (command "twitch-listen-only")
              { privileged = True
              , help = say "twitch-listen-only <channel>.  do not allow bot to respond in specified channel"
              , process = \rest -> modifyMS (\ms -> ms{writable = filter (/= rest) $ writable ms})
              }
          , (command "twitch-join")
              { privileged = True
              , help = say "twitch-join <channel>"
              , process = \rest -> do
                  chan <- readNick rest
                  debugM $ "attempting to join channel " ++ rest
                  lb $ send (joinChannel chan)
              }
          ]
    , moduleDefState = return $ TwitchState{password = Nothing, writable = []}
    }

----------------------------------------------------------------------
-- Encoding and decoding of messages

{- | 'encodeMessage' takes a message and converts it to a function.
   giving this function a string will attach the string to the message
   and output a string containing IRC protocol commands ready for writing
   on the outgoing stream socket.
-}
encodeMessage :: IrcMessage -> String -> String
encodeMessage msg =
  encodeTags (ircTags msg)
    . encodePrefix (ircMsgPrefix msg)
    . encodeCommand (ircMsgCommand msg)
    . encodeParams (ircMsgParams msg)

encodeTags :: [TwitchTag] -> String -> String
encodeTags [] = id
encodeTags tags = showChar '@' . showString' (concatMap encodeTag tags) . showChar ' '

encodeTag :: TwitchTag -> String
encodeTag (name, value) = name ++ "=" ++ value

encodePrefix :: String -> String -> String
encodePrefix [] = id
encodePrefix prefix = showChar ':' . showString' prefix . showChar ' '

encodeCommand :: String -> String -> String
encodeCommand = showString

encodeParams :: [String] -> String -> String
encodeParams [] = id
encodeParams (p : ps) = showChar ' ' . showString' p . encodeParams ps

-- IrcMessage is supposed to contain strings that are lists of bytes, but
-- if a plugin messes up the encoding then we may end up with arbitrary
-- Unicode codepoints. This is dangerous (\x10a would produce a newline!),
-- so we sanitize the message here.
showString' :: String -> String -> String
showString' = showString . map (\c -> if c > '\xFF' then '?' else c)

{- | 'decodeMessage' Takes an input line from the IRC protocol stream
   and decodes it into a message.  TODO: this has too many parameters.
-}
decodeMessage :: String -> String -> String -> IrcMessage
decodeMessage svr lbn line =
  IrcMessage
    { ircMsgServer = svr
    , ircMsgLBName = lbn
    , ircMsgPrefix = prefix
    , ircMsgCommand = cmd
    , ircMsgParams = params
    , ircDirection = Inbound
    , ircTags = tags
    }
 where
    (tags, rest0) = decodeTags (,) line
    (prefix, rest1) = decodePrefix (,) rest0
    (cmd, rest2) = decodeCmd (,) rest1
    params = decodeParams rest2

type TwitchTag = (String, String)

decodeTags :: ([TwitchTag] -> String -> ([TwitchTag], String)) -> String -> ([TwitchTag], String)
decodeTags k ('@' : cs) = let
  maybeIndex = elemIndex ' ' cs
  (parms, msg) = splitAt (fromJust maybeIndex) cs
  tags = map decodeTag $ splitOn ";" parms
  in k tags $ tail msg
decodeTags k cs = k [] cs

decodeTag :: String -> TwitchTag
decodeTag tagString = let
  tagParts = splitOn "=" tagString
  name = head tagParts
  value = last tagParts
  in (name, value)

decodePrefix :: (String -> String -> (String, String)) -> String -> (String, String)
decodePrefix k (':' : cs) = decodePrefix' k cs
decodePrefix k cs = k "" cs

decodePrefix' :: (String -> String -> (String, String)) -> String -> (String, String)
decodePrefix' j "" = j "" ""
decodePrefix' j (' ' : ds) = j "" ds
decodePrefix' j (c : ds) = decodePrefix' (j . (c :)) ds

decodeCmd :: (String -> String -> (String, String)) -> String -> (String, String)
decodeCmd k [] = k "" ""
decodeCmd k (' ' : cs) = k "" cs
decodeCmd k (c : cs) = decodeCmd (k . (c :)) cs

decodeParams :: String -> [String]
decodeParams = decodeParams' [] []

decodeParams' :: String -> [String] -> String -> [String]
decodeParams' param params []
  | null param = reverse params
  | otherwise = reverse (reverse param : params)
decodeParams' param params (' ' : cs)
  | null param = decodeParams' [] params cs
  | otherwise = decodeParams' [] (reverse param : params) cs
decodeParams' param params rest@(c@':' : cs)
  | null param = reverse (rest : params)
  | otherwise = decodeParams' (c : param) params cs
decodeParams' param params (c : cs) = decodeParams' (c : param) params cs

twitchSignOn :: String -> Nick -> Maybe String -> String -> LB ()
twitchSignOn svr nickn pwd ircname = do
  maybe (return ()) (delaySend 333 . pass (nTag nickn)) pwd
  delaySend 333 $ user (nTag nickn) (nName nickn) svr ircname
  delaySend 333 $ setNick nickn
  delaySend 333 $ capabilityRequestMessage (nTag nickn)

delaySend :: Int -> IrcMessage -> LB ()
delaySend milliseconds message = do
  threadDelay $ milliseconds * 1000
  send message

capabilityRequestMessage :: String -> IrcMessage
capabilityRequestMessage server = IrcMessage {
  ircMsgServer = server,
  ircMsgCommand = "CAP REQ",
  ircMsgParams = [":twitch.tv/tags", "twitch.tv/commands", "twitch.tv/membership"],
  ircMsgPrefix = "",
  ircMsgLBName = "",
  ircTags = [],
  ircDirection = Outbound
}
------------------------------------------------------------------------
--
-- Lambdabot is mostly synchronous.  We have a main loop, which reads
-- messages and forks threads to execute commands (which write responses).
-- OR
-- We have a main loop which reads offline commands, and synchronously
-- interprets them.

doRetry :: Int -> HostName -> PortID -> String -> String -> Maybe String -> String -> ModuleT TwitchState LB ()
doRetry delay hostn portnum tag nickn psw ui = do
  continue <- lift $ gets $ \st -> M.member tag (ircPersists st) && not (M.member tag $ ircServerMap st)
  if continue
    then let
      tryToGoOnline = goOnline hostn portnum tag nickn psw ui
      exception = showErrorAndRetry delay hostn portnum tag nickn psw ui
      in tryToGoOnline `E.catch` exception
    else do
      chans <- lift $ gets ircChannels
      forM_ (M.keys chans) $
        \chan -> when (nTag (getCN chan) == tag) $
          lift $
            modify $
              \state' -> state'{ircChannels = M.delete chan $ ircChannels state'}

showErrorAndRetry :: Int -> HostName -> PortID -> String -> String -> Maybe String -> String -> SomeException -> ModuleT TwitchState LB ()
showErrorAndRetry delay hostn portnum tag nickn psw ui e@SomeException{} = do
  errorM (show e)
  io $ threadDelay delay
  doRetry delay hostn portnum tag nickn psw ui

goOnline :: HostName -> PortID -> String -> String -> Maybe String -> String -> ModuleT TwitchState LB ()
goOnline hostn portnum tag nickn psw ui = do
  sock <- io $ connectTo hostn portnum
  io $ hSetBuffering sock NoBuffering
  -- Implements flood control: RFC 2813, section 5.8
  sem1 <- io $ SSem.new 0
  sem2 <- io $ SSem.new 4 -- one extra token stays in the MVar
  sendmv <- io newEmptyMVar
  io . void . fork . forever $ do
    SSem.wait sem1
    threadDelay 2000000
    SSem.signal sem2
  io . void . fork . forever $ do
    SSem.wait sem2
    putMVar sendmv ()
    SSem.signal sem1
  fin <- io $ SSem.new 0
  interactiveChannels <- withMS $ \ms _ -> do
    pure $ writable ms
  E.catch (registerServer tag (io . sendMsg interactiveChannels sock sendmv fin)) (\err@SomeException{} -> io (hClose sock) >> E.throwIO err)
  lb $ twitchSignOn hostn (Nick tag nickn) psw ui
  ready <- io $ SSem.new 0
  lb $ void $ forkFinally (readerLoop tag nickn sock ready `E.catch` handleReaderLoopException) (const $ io $ SSem.signal fin)
  void $
    fork $ do
      io $ SSem.wait fin
      unregisterServer tag
      io $ hClose sock
      io $ SSem.signal ready
      delay <- getConfig reconnectDelay
      doRetry delay hostn portnum tag nickn psw ui
  watch <- io $
    fork $ do
      threadDelay 15000000
      errorM "Welcome timeout!"
      SSem.signal fin
  io $ SSem.wait ready
  killThread watch

online :: String -> String -> PortID -> String -> String -> Twitch ()
online tag hostn portnum nickn ui = do
  pwd <- password <$> readMS
  modifyMS $ \ms -> ms{password = Nothing}
  goOnline hostn portnum tag nickn pwd ui

readerLoop :: String -> String -> Handle -> SSem.SSem -> LB ()
readerLoop tag nickn sock ready = forever $ do
  line <- io $ hGetLine sock
  let singleLine = filter (`notElem` "\r\n") line
  if "PING" `isPrefixOf` singleLine
    then io $ P.hPut sock $ P.pack "PONG\r\n"
    else void . fork . void . timeout 15000000 $ do
      let msg = decodeMessage tag nickn singleLine
      debugM $ show msg
      when (ircMsgCommand msg == "001") $ io $ SSem.signal ready
      received msg

handleReaderLoopException :: MonadLogging m => SomeException -> m ()
handleReaderLoopException (SomeException e) = case show $ typeOf e of
  "IOException" -> infoM "Ignoring io exception from reader loop."
  _ -> errorM $ show e ++ show (typeOf e)

sendMsg :: [String] -> Handle -> MVar () -> SSem.SSem -> IrcMessage -> IO ()
sendMsg interactiveChannels sock mv fin msg =
  let tryToSend = maybeSendEncodedMessage interactiveChannels msg $ putEncodedMessage sock mv
      exception = handleSocketPutError fin
   in tryToSend `E.catch` exception

maybeSendEncodedMessage :: [String] -> IrcMessage -> (IrcMessage -> IO ()) -> IO ()
maybeSendEncodedMessage interactiveChannels msg write =
  let controlMessage = ircMsgCommand msg /= "PRIVMSG"
      interactiveChannel = head (ircMsgParams msg) `elem` interactiveChannels
      shouldRespond = controlMessage || interactiveChannel
   in when shouldRespond $ write msg

putEncodedMessage :: Handle -> MVar () -> IrcMessage -> IO ()
putEncodedMessage sock mv msg = do
  debugM $ show msg
  takeMVar mv
  P.hPut sock $ P.pack $ encodeMessage msg "\r\n"

handleSocketPutError :: SSem.SSem -> IOError -> IO ()
handleSocketPutError fin err = do
  errorM $ show (err :: IOError)
  SSem.signal fin
