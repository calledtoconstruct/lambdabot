
{-# OPTIONS_GHC -fno-warn-overlapping-patterns -Wmissing-signatures #-}

module Lambdabot.Plugin.Twitch.Twitch (
  twitchPlugin
) where

import Lambdabot.IRC
import Lambdabot.Logging
import Lambdabot.Monad
import Lambdabot.Plugin
import Lambdabot.Util
import Lambdabot.Config.Twitch

import Control.Concurrent.Lifted
import qualified Control.Concurrent.SSem as SSem
import Control.Exception.Lifted as E (SomeException(..), throwIO, catch)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import qualified Data.ByteString.Char8 as P
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Network( connectTo, PortID(..), HostName )
import System.IO
import System.Timeout.Lifted

data IRCState = IRCState {
  password :: Maybe String
}

type IRC = ModuleT IRCState LB

twitchPlugin :: Module IRCState
twitchPlugin = newModule {
  moduleCmds = return [
    (command "twitch-connect") {
      privileged = True,
      help = say "twitch-connect tag host portnum nickname userinfo.  connect to a twitch server",
      process = \rest ->
        case splitOn " " rest of
          tag:hostn:portn:nickn:uix -> do
            pn <- (PortNumber . fromInteger) `fmap` readM portn
            lift (online tag hostn pn nickn (intercalate " " uix))
          _ -> say "Not enough parameters!"
    },
    (command "twitch-persist-connect") {
      privileged = True,
      help = say "twitch-persist-connect tag host portnum nickname userinfo.  connect to a twitch server and reconnect on network failures",
      process = \rest ->
        case splitOn " " rest of
          tag:hostn:portn:nickn:uix -> do
            pn <- (PortNumber . fromInteger) `fmap` readM portn
            lift (online tag hostn pn nickn (intercalate " " uix))
            lift $ lift $ modify $ \state' -> state' { ircPersists = M.insert tag True $ ircPersists state' }
          _ -> say "Not enough parameters!"
    },
    (command "twitch-auth") {
      privileged = True,
      help = say "twitch-auth oauth.  set oauth for next twitch-connect command",
      process = \rest ->
        case splitOn " " rest of
          pwd:_ -> do
            modifyMS (\ms -> ms{ password = Just pwd })
          _ -> say "Not enough parameters!"
    },
    (command "twitch-join") {
      privileged = True,
      help = say "twitch-join <channel>",
      process = \rest -> do
        chan <- readNick rest
        debugM $ "attempting to join channel " ++ rest
        lb $ send (joinChannel chan)
    }
  ],
  moduleDefState = return $ IRCState { password = Nothing }
}

----------------------------------------------------------------------
-- Encoding and decoding of messages

-- | 'encodeMessage' takes a message and converts it to a function.
--   giving this function a string will attach the string to the message
--   and output a string containing IRC protocol commands ready for writing
--   on the outgoing stream socket.
encodeMessage :: IrcMessage -> String -> String
encodeMessage msg = encodePrefix (ircMsgPrefix msg) . encodeCommand (ircMsgCommand msg) . encodeParams (ircMsgParams msg)

encodePrefix :: String -> String -> String
encodePrefix [] = id
encodePrefix prefix = showChar ':' . showString' prefix . showChar ' '

encodeCommand :: String -> String -> String
encodeCommand cmd = showString cmd

encodeParams :: [String] -> String -> String
encodeParams [] = id
encodeParams (p:ps) = showChar ' ' . showString' p . encodeParams ps

-- IrcMessage is supposed to contain strings that are lists of bytes, but
-- if a plugin messes up the encoding then we may end up with arbitrary
-- Unicode codepoints. This is dangerous (\x10a would produce a newline!),
-- so we sanitize the message here.
showString' :: String -> String -> String
showString' = showString . map (\c -> if c > '\xFF' then '?' else c)

-- | 'decodeMessage' Takes an input line from the IRC protocol stream
--   and decodes it into a message.  TODO: this has too many parameters.
decodeMessage :: String -> String -> String -> IrcMessage
decodeMessage svr lbn line = IrcMessage {
  ircMsgServer = svr,
  ircMsgLBName = lbn,
  ircMsgPrefix = prefix,
  ircMsgCommand = cmd,
  ircMsgParams = params
}
  where (prefix, rest1) = decodePrefix (,) line
        (cmd, rest2)    = decodeCmd (,) rest1
        params          = decodeParams rest2

decodePrefix :: (String -> String -> (String, String)) -> String -> (String, String)
decodePrefix k (':':cs) = decodePrefix' k cs
decodePrefix k cs = k "" cs

decodePrefix' :: (String -> String -> (String, String)) -> String -> (String, String)
decodePrefix' j ""       = j "" ""
decodePrefix' j (' ':ds) = j "" ds
decodePrefix' j (c:ds)   = decodePrefix' (j . (c:)) ds

decodeCmd :: (String -> String -> (String, String)) -> String -> (String, String)
decodeCmd k []       = k "" ""
decodeCmd k (' ':cs) = k "" cs
decodeCmd k (c:cs)   = decodeCmd (k . (c:)) cs

decodeParams :: String -> [String]
decodeParams xs = decodeParams' [] [] xs

decodeParams' :: String -> [String] -> String -> [String]
decodeParams' param params []
  | null param = reverse params
  | otherwise  = reverse (reverse param : params)
decodeParams' param params (' ' : cs)
  | null param = decodeParams' [] params cs
  | otherwise  = decodeParams' [] (reverse param : params) cs
decodeParams' param params rest@(c@':' : cs)
  | null param = reverse (rest : params)
  | otherwise  = decodeParams' (c:param) params cs
decodeParams' param params (c:cs) = decodeParams' (c:param) params cs

twitchSignOn :: String -> Nick -> Maybe String -> String -> LB ()
twitchSignOn svr nickn pwd ircname = do
  maybe (return ()) (\pwd' -> send $ pass (nTag nickn) pwd') pwd
  send $ user (nTag nickn) (nName nickn) svr ircname
  send $ setNick nickn

------------------------------------------------------------------------
--
-- Lambdabot is mostly synchronous.  We have a main loop, which reads
-- messages and forks threads to execute commands (which write responses).
-- OR
-- We have a main loop which reads offline commands, and synchronously
-- interprets them.

doRetry :: HostName -> PortID -> String -> Int -> String -> Maybe String -> String -> ModuleT st LB ()
doRetry hostn portnum tag delay nickn psw ui = do
  continue <- lift $ gets $ \st -> (M.member tag $ ircPersists st) && not (M.member tag $ ircServerMap st)
  if continue then do
    E.catch (goOnline hostn portnum tag nickn psw ui) (\e@SomeException{} -> do
      errorM (show e)
      io $ threadDelay delay
      doRetry hostn portnum tag delay nickn psw ui)
    else do
      chans <- lift $ gets ircChannels
      forM_ (M.keys chans) $ \chan -> when (nTag (getCN chan) == tag) $ lift $ modify $ \state' -> state' { ircChannels = M.delete chan $ ircChannels state' }

goOnline :: HostName -> PortID -> String -> String -> Maybe String -> String -> ModuleT st LB ()
goOnline hostn portnum tag nickn psw ui = do
  sock    <- io $ connectTo hostn portnum
  io $ hSetBuffering sock NoBuffering
  -- Implements flood control: RFC 2813, section 5.8
  sem1    <- io $ SSem.new 0
  sem2    <- io $ SSem.new 4 -- one extra token stays in the MVar
  sendmv  <- io newEmptyMVar
  io . void . fork . forever $ do
    SSem.wait sem1
    threadDelay 2000000
    SSem.signal sem2
  io . void . fork . forever $ do
    SSem.wait sem2
    putMVar sendmv ()
    SSem.signal sem1
  fin <- io $ SSem.new 0
  E.catch (registerServer tag (io . sendMsg sock sendmv fin)) (\err@SomeException{} -> io (hClose sock) >> E.throwIO err)
  lb $ twitchSignOn hostn (Nick tag nickn) psw ui
  ready <- io $ SSem.new 0
  lb $ void $ forkFinally
    (E.catch (readerLoop tag nickn sock ready) (\e@SomeException{} -> errorM (show e)))
    (const $ io $ SSem.signal fin)
  void $ fork $ do
    io $ SSem.wait fin
    unregisterServer tag
    io $ hClose sock
    io $ SSem.signal ready
    delay <- getConfig reconnectDelay
    doRetry hostn portnum tag delay nickn psw ui
  watch <- io $ fork $ do
    threadDelay 10000000
    errorM "Welcome timeout!"
    SSem.signal fin
  io $ SSem.wait ready
  killThread watch

online :: String -> String -> PortID -> String -> String -> IRC ()
online tag hostn portnum nickn ui = do
  pwd <- password `fmap` readMS
  modifyMS $ \ms -> ms { password = Nothing }
  goOnline hostn portnum tag nickn pwd ui

readerLoop :: String -> String -> Handle -> SSem.SSem -> LB ()
readerLoop tag nickn sock ready = forever $ do
    line <- io $ hGetLine sock
    let line' = filter (`notElem` "\r\n") line
    debugM $ "Received from " ++ tag ++ " :: " ++ nickn ++ " >> " ++ line'
    if "PING" `isPrefixOf` line' then io $ P.hPut sock $ P.pack $ "PONG\r\n" else void . fork . void . timeout 15000000 $ do
      let msg = decodeMessage tag nickn line'
      when (ircMsgCommand msg == "001") $ io $ SSem.signal ready
      received msg
                
sendMsg :: Handle -> MVar () -> SSem.SSem -> IrcMessage -> IO ()
sendMsg sock mv fin msg = E.catch (do
  takeMVar mv
  P.hPut sock $ P.pack $ encodeMessage msg "\r\n") (\err -> do
    errorM (show (err :: IOError))
    SSem.signal fin)
